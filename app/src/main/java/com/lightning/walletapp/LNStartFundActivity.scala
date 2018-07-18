package com.lightning.walletapp

import spray.json._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import scala.collection.JavaConverters._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.Denomination._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap
import com.lightning.walletapp.lnutils.olympus.CloudAct
import com.lightning.walletapp.ln.Scripts.pubKeyScript
import com.lightning.walletapp.helper.AES
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.wallet.SendRequest
import android.app.AlertDialog
import java.util.Collections
import android.os.Bundle

import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import org.bitcoinj.core.{Coin, TransactionOutput}
import android.widget.{ImageButton, TextView}
import scala.util.{Failure, Success, Try}


class LNStartFundActivity extends TimerActivity { me =>
  lazy val lnStartFundCancel = findViewById(R.id.lnStartFundCancel).asInstanceOf[ImageButton]
  lazy val lnStartFundDetails = findViewById(R.id.lnStartFundDetails).asInstanceOf[TextView]
  var whenBackPressed: Runnable = UITask(super.onBackPressed)
  override def onBackPressed = whenBackPressed.run

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_start_fund)

    app.TransData.value match {
      case remoteView @ RemoteNodeView(ann \ _) => proceed(remoteView.asString(nodeFundView, "<br>"), ann)
      case hardcodedView @ HardcodedNodeView(ann, _) => proceed(hardcodedView.asString(nodeFundView, "<br>"), ann)
      case ann: NodeAnnouncement => proceed(HardcodedNodeView(ann, chansNumber.last).asString(nodeFundView, "<br>"), ann)
      case _ => finish
    }

    app.TransData.value = null
    // Or back if resources are freed
  } else me exitTo classOf[MainActivity]

  def proceed(asString: String, ann: NodeAnnouncement) = {
    val freshChan = app.ChannelManager.createChannel(Set.empty, InitData apply ann)
    lnStartFundCancel setOnClickListener onButtonTap(whenBackPressed.run)
    lnStartFundDetails setText asString.html

    class OpenListener extends ConnectionListener with ChannelListener {
      val noLossProtect = new LightningException(me getString err_ln_no_data_loss_protect)
      val peerOffline = new LightningException(me getString err_ln_peer_offline format ann.addresses.head.toString)
      override def onIncompatible(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> noLossProtect)
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == ann.nodeId) onException(freshChan -> peerOffline)

      override def onMessage(nodeId: PublicKey, msg: LightningMessage) = msg match {
        case err: Error if nodeId == ann.nodeId => onException(freshChan -> err.exception)
        case msg: ChannelSetupMessage if nodeId == ann.nodeId => freshChan process msg
        case _ =>
      }

      override def onException = {
        case _ \ errorWhileOpening =>
          // Inform user, disconnect this channel, go back
          UITask(app toast errorWhileOpening.getMessage).run
          whenBackPressed.run
      }
    }

    def saveChan(some: HasCommitments) = {
      // First of all we should store this chan
      // error here will halt all further progress
      freshChan STORE some

      // Start watching a channel funding script and save a channel
      val fundingScript = some.commitments.commitInput.txOut.publicKeyScript
      app.kit.wallet.addWatchedScripts(Collections singletonList fundingScript)

      // Attempt to save a channel on the cloud right away
      val refund = RefundingData(some.announce, None, some.commitments)
      val encrypted = AES.encode(refund.toJson.toString, LNParams.cloudSecret)
      val act = CloudAct(encrypted, Seq("key" -> LNParams.cloudId.toString), "data/put")
      OlympusWrap tellClouds act
    }

    def localOpenListener = new OpenListener {
      override def onOperational(remotePeerCandidateNodeId: PublicKey) =
        // Remote peer has sent their Init so we ask user to provide an amount
        if (remotePeerCandidateNodeId == ann.nodeId) askLocalFundingConfirm.run

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we create a real funding transaction
          val realKey = pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          // We create a funding transaction by replacing an output with a real one in a saved dummy funding transaction
          val realOut = new TransactionOutput(app.params, null, Coin valueOf cmd.realFundingAmountSat, realKey.getProgram)
          val withReplacedDummy = cmd.dummyRequest.tx.getOutputs.asScala.patch(cmd.outIndex, List(realOut), 1)

          cmd.dummyRequest.tx.clearOutputs
          for (out <- withReplacedDummy) cmd.dummyRequest.tx addOutput out
          freshChan process CMDFunding(app.kit.sign(cmd.dummyRequest).tx)

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Preliminary negotiations are complete, save channel and broadcast a fund tx
          saveChan(wait)

          // Attach normal listeners and add this channel to runtime list
          freshChan.listeners = app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan

          // Broadcast a funding transaction
          // Tell wallet activity to redirect to ops
          LNParams.broadcaster nullOnBecome freshChan
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }

      private def askLocalFundingConfirm = UITask {
        val maxCap = MilliSatoshi(math.min(app.kit.conf0Balance.value, 16777215L) * 1000L)
        val minCap = MilliSatoshi(math.max(LNParams.broadcaster.perKwThreeSat, 300000L) * 1000L)
        val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
        val txt = getString(amount_hint_newchan).format(denom withSign minCap, denom withSign maxCap)
        val rateManager = new RateManager(txt, content)
        val dummyKey = randomPrivKey.publicKey

        def next(msat: MilliSatoshi) = new TxProcessor {
          val dummyScript = pubKeyScript(dummyKey, dummyKey)
          val pay = P2WSHData(msat, dummyScript)

          def futureProcess(unsignedRequest: SendRequest) = {
            val finder = new PubKeyScriptIndexFinder(unsignedRequest.tx)
            val outIndex = finder.findPubKeyScriptIndex(dummyScript, None)
            val realChannelFundingAmountSat = unsignedRequest.tx.getOutput(outIndex).getValue.getValue
            val theirUnspendableReserveSat = realChannelFundingAmountSat / LNParams.theirReserveToFundingRatio
            val finalPubKeyScript: BinaryData = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
            val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
            freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, LNParams.broadcaster.perKwThreeSat,
              pushMsat = 0L, dummyRequest = unsignedRequest, outIndex, realChannelFundingAmountSat)
          }

          def onTxFail(fundingError: Throwable) = {
            val bld = baseBuilder(title = messageWhenMakingTx(fundingError), null)
            mkCheckForm(alert => rm(alert)(finish), none, bld, dialog_ok, dialog_cancel)
          }
        }

        def askAttempt(alert: AlertDialog) = rateManager.result match {
          case Success(ms) if ms < minCap => app toast dialog_sum_small
          case Success(ms) if ms > maxCap => app toast dialog_sum_big
          case Failure(reason) => app toast dialog_sum_empty
          case Success(ms) => rm(alert)(next(ms).start)
        }

        def useMax(alert: AlertDialog) = rateManager setSum Try(maxCap)
        val bld = baseBuilder(getString(ln_ops_start_fund_local_title).html, content)
        mkCheckFormNeutral(askAttempt, none, useMax, bld, dialog_next, dialog_cancel, dialog_max)
      }
    }

    def remoteOpenListener(wsw: WSWrap) = new OpenListener {
      override def onOperational(remotePeerCandidateNodeId: PublicKey) =
        // #1 peer has provided an Init so we reassure that Funder is there
        if (remotePeerCandidateNodeId == ann.nodeId) wsw send wsw.params.start

      override def onProcessSuccess = {
        case (_, _: InitData, started: Started) if started.start == wsw.params.start =>
          // #2 funder has returned a Started which is identical to the one we have
          askExternalFundingConfirm(started).run

        case (_, wait: WaitBroadcastRemoteData, sent: FundingTxBroadcasted) =>
          // #5 we have got a funder confirmation sooner than an on-chain event
          app.kit.blockSend(sent.tx)
      }

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // #3 peer has accepted our proposal and we have all the data to request a funding tx
          val realKey = pubKeyScript(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
          wsw send PrepareFundingTx(wsw.params.userId, realKey)

        case (_, wait: WaitBroadcastRemoteData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // #4 peer has signed our first commit so we can ask funder to broadcast a tx
          wsw send BroadcastFundingTx(wsw.params.userId, txHash = wait.txHash)
          freshChan.listeners ++= app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan
          saveChan(wait)

        case (_, _: WaitFundingDoneData, WAIT_FUNDING_DONE, WAIT_FUNDING_DONE) =>
          // #6 we have received a remote funding tx and may exit to ops page
          app.TransData.value = FragWallet.REDIRECT
          me exitTo classOf[WalletActivity]
      }

      private def askExternalFundingConfirm(started: Started) = UITask {
        val capacity \ fundingFee = coloredIn(started.start.fundingAmount) -> coloredOut(started.fee)
        val content = getString(ex_fund_accept).format(started.start.host, capacity, fundingFee)
        val bld = baseBuilder(getString(ln_ops_start_fund_external_title).html, content.html)
        mkCheckForm(sendCMDOpenChannel, none, bld, dialog_next, dialog_cancel)
      }

      private def sendCMDOpenChannel(alert: AlertDialog) = rm(alert) {
        val finalPubKeyScript: BinaryData = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
        val theirUnspendableReserveSat = wsw.params.start.fundingAmount.amount / LNParams.theirReserveToFundingRatio
        val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
        freshChan process CMDOpenChannel(localParams, tempChanId = random getBytes 32, LNParams.broadcaster.perKwThreeSat,
          pushMsat = 0L, dummyRequest = null, outIndex = -1, wsw.params.start.fundingAmount.amount)
      }
    }

    val efListener = new ExternalFunderListener {
      override def onDisconnect: Unit = whenBackPressed.run
      override def onMessage(msg: FundMsg) = freshChan process msg
    }

    val openListener = ExternalFunder.worker match {
      case Some(workingWsw) => remoteOpenListener(workingWsw)
      case None => localOpenListener
    }

    whenBackPressed = UITask {
      freshChan.listeners -= openListener
      ConnectionManager.listeners -= openListener
      for (wsw <- ExternalFunder.worker) wsw.listeners -= efListener
      // Worker may have already been removed on some connection failure
      ConnectionManager.connections.get(ann.nodeId).foreach(_.disconnect)
      finish
    }

    // Wire up listeners
    freshChan.listeners += openListener
    ConnectionManager.listeners += openListener
    // Attempt to set a listener irregardles of whether it exists
    for (wsw <- ExternalFunder.worker) wsw.listeners += efListener
    ConnectionManager.connectTo(ann, notify = true)
  }
}