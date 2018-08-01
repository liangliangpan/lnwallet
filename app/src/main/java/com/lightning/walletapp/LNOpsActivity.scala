package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import fr.acinq.bitcoin.{BinaryData, Satoshi}
import android.view.{Menu, MenuItem, View, ViewGroup}
import org.bitcoinj.core.{Block, FilteredBlock, Peer}
import com.lightning.walletapp.ln.{Channel, ChannelData, RefundingData}
import github.nisrulz.stackedhorizontalprogressbar.StackedHorizontalProgressBar
import com.lightning.walletapp.lnutils.IconGetter.scrWidth
import com.lightning.walletapp.lnutils.PaymentTable
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.ln.Tools.wrap
import android.support.v7.widget.Toolbar
import android.os.Bundle
import java.util.Date


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  val localChanCache = for (c <- app.ChannelManager.all if me canDisplay c.data) yield c
  lazy val presentChans = app.getResources getStringArray R.array.ln_chan_present
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val otherState = getString(ln_info_status_other)
  lazy val fundingInfo = getString(ln_info_funding)
  lazy val host = me

  val adapter = new BaseAdapter {
    def getItem(position: Int) = localChanCache(position)
    def getItemId(chanPosition: Int) = chanPosition
    def getCount = localChanCache.size

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.chan_card, null) else savedView
      val holder = if (null == card.getTag) new ViewHolder(card) else card.getTag.asInstanceOf[ViewHolder]
      holder fillView getItem(position)
      card
    }
  }

  val becomeListener = new ChannelListener {
    override def onBecome: PartialFunction[Transition, Unit] = {
      case anyStateChange => UITask(adapter.notifyDataSetChanged).run
    }
  }

  val blocksListener = new BlocksListener {
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) =
      if (left < 1) UITask(adapter.notifyDataSetChanged).run
  }

  class ViewHolder(view: View) {
    val stackBar = view.findViewById(R.id.stackBar).asInstanceOf[StackedHorizontalProgressBar]
    val stateAndConnectivity = view.findViewById(R.id.stateAndConnectivity).asInstanceOf[TextView]
    val addressAndKey = view.findViewById(R.id.addressAndKey).asInstanceOf[TextView]
    val extraInfoText = view.findViewById(R.id.extraInfoText).asInstanceOf[TextView]
    val extraInfo = view.findViewById(R.id.extraInfo)

    val wrappers =
      view.findViewById(R.id.refundableAmount) ::
        view.findViewById(R.id.paymentsInFlight) ::
        view.findViewById(R.id.paymentsReceived) ::
        view.findViewById(R.id.totalCapacity) ::
        view.findViewById(R.id.paymentsSent) ::
        view.findViewById(R.id.fundingDepth) ::
        view.findViewById(R.id.canReceive) ::
        view.findViewById(R.id.startedAt) ::
        view.findViewById(R.id.refundFee) ::
        view.findViewById(R.id.closedAt) ::
        view.findViewById(R.id.canSend) ::
        stackBar :: Nil

    val refundableAmountText = view.findViewById(R.id.refundableAmountText).asInstanceOf[TextView]
    val paymentsInFlightText = view.findViewById(R.id.paymentsInFlightText).asInstanceOf[TextView]
    val paymentsReceivedText = view.findViewById(R.id.paymentsReceivedText).asInstanceOf[TextView]
    val totalCapacityText = view.findViewById(R.id.totalCapacityText).asInstanceOf[TextView]
    val paymentsSentText = view.findViewById(R.id.paymentsSentText).asInstanceOf[TextView]
    val fundingDepthText = view.findViewById(R.id.fundingDepthText).asInstanceOf[TextView]
    val canReceiveText = view.findViewById(R.id.canReceiveText).asInstanceOf[TextView]
    val startedAtText = view.findViewById(R.id.startedAtText).asInstanceOf[TextView]
    val refundFeeText = view.findViewById(R.id.refundFeeText).asInstanceOf[TextView]
    val closedAtText = view.findViewById(R.id.closedAtText).asInstanceOf[TextView]
    val canSendText = view.findViewById(R.id.canSendText).asInstanceOf[TextView]
    extraInfo setVisibility View.GONE
    stackBar setMax 1000
    view setTag this

    def visibleExcept(gone: Int*) =
      for (textWrapper <- wrappers) {
        val isGone = gone contains textWrapper.getId
        textWrapper setVisibility viewMap(!isGone)
      }

    def fillView(chan: Channel) = {
      val state = stateStatusColor(chan)
      val connectivity = connectivityStatusColor(chan)
      addressAndKey setText chan.data.announce.toString.html
      stateAndConnectivity setText s"$state<br>$connectivity".html

      chan { cs =>
        val capacity = cs.commitInput.txOut.amount
        val started = me time new Date(cs.startedAt)
        val commitFee = Satoshi(cs.reducedRemoteState.feesSat)
        val threshold = math.max(cs.remoteParams.minimumDepth, LNParams.minDepth)
        val txDepth \ _ = LNParams.broadcaster.getStatus(Commitments fundingTxid cs)
        val canSendMsat \ canReceiveMsat = estimateCanSend(chan) -> estimateCanReceive(chan)
        val valueInFlight = Satoshi(inFlightHtlcs(chan).map(_.add.amount.amount).sum / 1000L)
        val refundable = Satoshi(Commitments.latestRemoteCommit(cs).spec.toRemoteMsat / 1000L)
        val canNotReceive = txDepth > 6 && channelAndHop(chan).isEmpty
        val valueReceived = Satoshi(getStat(cs.channelId, 1) / 1000L)
        val valueSent = Satoshi(getStat(cs.channelId, 0) / 1000L)

        stackBar setProgress (canSendMsat / capacity.amount).toInt
        fundingDepthText setText fundingInfo.format(txDepth, threshold)
        stackBar setSecondaryProgress (canReceiveMsat / capacity.amount).toInt
        canReceiveText setText denom.withSign(Satoshi(canReceiveMsat) / 1000L).html
        canSendText setText denom.withSign(Satoshi(canSendMsat) / 1000L).html
        refundableAmountText setText denom.withSign(refundable).html
        totalCapacityText setText denom.withSign(capacity).html
        refundFeeText setText denom.withSign(commitFee).html
        startedAtText setText started.html

        paymentsReceivedText setText sumOrNothing(valueReceived).html
        paymentsInFlightText setText sumOrNothing(valueInFlight).html
        paymentsSentText setText sumOrNothing(valueSent).html

        chan.data match {
          case _: WaitFundingDoneData =>
            visibleExcept(R.id.stackBar, R.id.canSend, R.id.canReceive, R.id.closedAt,
              R.id.paymentsSent, R.id.paymentsReceived, R.id.paymentsInFlight)

          case remote: WaitBroadcastRemoteData =>
            if (remote.fail.isDefined) extraInfo setVisibility View.VISIBLE
            if (remote.fail.isDefined) extraInfoText setText remote.fail.get.reason
            visibleExcept(R.id.stackBar, R.id.canSend, R.id.canReceive, R.id.closedAt,
              R.id.paymentsSent, R.id.paymentsReceived, R.id.paymentsInFlight)

          case _: NormalData if isOperational(chan) =>
            if (canNotReceive) extraInfo setVisibility View.VISIBLE
            if (canNotReceive) extraInfoText setText ln_info_no_receive
            visibleExcept(R.id.fundingDepth, R.id.closedAt)

          case _: NormalData | _: NegotiationsData =>
            extraInfoText setText ln_info_coop_attempt
            extraInfo setVisibility View.VISIBLE

            visibleExcept(R.id.stackBar, R.id.canSend, R.id.canReceive,
              R.id.refundFee, R.id.fundingDepth, R.id.closedAt)

          case cd: ClosingData =>
            val closed = me time new Date(cd.closedAt)
            extraInfo setVisibility View.VISIBLE
            extraInfoText setText closedBy(cd)
            closedAtText setText closed.html

            visibleExcept(R.id.stackBar, R.id.canSend, R.id.canReceive,
              R.id.refundFee, R.id.paymentsSent, R.id.paymentsReceived,
              R.id.paymentsInFlight, R.id.fundingDepth)

          case otherwise =>
            visibleExcept(R.id.stackBar, R.id.totalCapacity, R.id.canSend,
              R.id.canReceive, R.id.refundFee, R.id.closedAt, R.id.paymentsSent,
              R.id.paymentsReceived, R.id.paymentsInFlight, R.id.fundingDepth)
        }
      }
    }
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.peerGroup removeBlocksDownloadedEventListener blocksListener
    for (chan <- localChanCache) chan.listeners -= becomeListener
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    if (m.getItemId == R.id.actionAddEntity) me exitTo classOf[LNStartActivity]
    true
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.add_entity, menu)
    true
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_ops)
    wrap(gridView setAdapter adapter)(getSupportActionBar setTitle action_ln_details)
    getSupportActionBar setSubtitle app.plurOrZero(presentChans, localChanCache.size)
    app.kit.peerGroup addBlocksDownloadedEventListener blocksListener
    for (chan <- localChanCache) chan.listeners += becomeListener
    gridView setNumColumns math.round(scrWidth / 2.4).toInt
  } else me exitTo classOf[MainActivity]

  // UTILS

  def stateStatusColor(c: Channel): String = c.state match {
    case OPEN if isOperational(c) => me getString ln_info_status_open
    case WAIT_FUNDING_DONE => me getString ln_info_status_opening
    case NEGOTIATIONS => me getString ln_info_status_negotiations
    case OPEN => me getString ln_info_status_shutdown
    case _ => otherState format c.state
  }

  def connectivityStatusColor(c: Channel) = c.state match {
    case OFFLINE | CLOSING => me getString ln_info_state_offline
    case _ => me getString ln_info_state_online
  }

  def closedBy(cd: ClosingData) =
    if (cd.remoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.nextRemoteCommit.nonEmpty) me getString ln_info_close_remote
    else if (cd.mutualClose.nonEmpty) me getString ln_info_close_coop
    else me getString ln_info_close_local

  def canDisplay(chanData: ChannelData) = chanData match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case otherwise => true
  }

  def sumOrNothing(sum: Satoshi) = sum match {
    case Satoshi(0L) => me getString ln_info_nothing
    case something => denom withSign something
  }

  def getStat(chanId: BinaryData, direction: Int) = {
    // Direction = 0 = untgoing = lastMast, = 1 = incoming = firstMsat
    val cursor = LNParams.db.select(PaymentTable.selectStatSql, chanId, direction)
    RichCursor(cursor) headTry { case RichCursor(с1) => с1 getLong direction } getOrElse 0L
  }
}