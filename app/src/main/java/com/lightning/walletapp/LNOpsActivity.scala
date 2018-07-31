package com.lightning.walletapp

import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln._
import android.os.Bundle
import com.lightning.walletapp.R.string._
import android.support.v7.widget.Toolbar
import android.view.{View, ViewGroup}
import android.widget._
import java.util.Date

import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.ln.Channel._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.{Channel, ChannelData, RefundingData}
import com.lightning.walletapp.lnutils.PaymentTable
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi}
import github.nisrulz.stackedhorizontalprogressbar.StackedHorizontalProgressBar


class LNOpsActivity extends TimerActivity with HumanTimeDisplay { me =>
  val localChanCache = for (chan <- app.ChannelManager.all if me canDisplay chan.data) yield chan
  val onlineChans = app.getResources getStringArray R.array.ln_chan_online
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  lazy val host = me

  class ViewHolder(view: View) {
    val stackBar = view.findViewById(R.id.stackBar).asInstanceOf[StackedHorizontalProgressBar]
    val stateAndConnectivity = view.findViewById(R.id.stateAndConnectivity).asInstanceOf[TextView]
    val addressAndKey = view.findViewById(R.id.addressAndKey).asInstanceOf[TextView]

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
    stackBar setMax 1000

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
        val valueInFlight = Satoshi(inFlightHtlcs(chan).map(_.add.amount.amount).sum / 1000L)
        val refundable = Satoshi(Commitments.latestRemoteCommit(cs).spec.toRemoteMsat / 1000L)
        val canSendMsat \ canReceiveMsat = estimateCanSend(chan) -> estimateCanReceive(chan)
        val txDepth \ _ = LNParams.broadcaster.getStatus(Commitments fundingTxid cs)
        val valueReceived = Satoshi(getStat(cs.channelId, 1).getOrElse(0L) / 1000L)
        val valueSent = Satoshi(getStat(cs.channelId, 0).getOrElse(0L) / 1000)

        stackBar setProgress (canSendMsat / capacity.amount).toInt
        stackBar setSecondaryProgress (canReceiveMsat / capacity.amount).toInt
        canReceiveText setText denom.withSign(Satoshi apply canReceiveMsat / 1000L).html
        canSendText setText denom.withSign(Satoshi apply canSendMsat / 1000L).html
        paymentsReceivedText setText denom.withSign(valueReceived).html
        paymentsInFlightText setText denom.withSign(valueInFlight).html
        refundableAmountText setText denom.withSign(refundable).html
        totalCapacityText setText denom.withSign(capacity).html
        paymentsSentText setText denom.withSign(valueSent).html
        refundFeeText setText denom.withSign(commitFee).html
        fundingDepthText setText s"$txDepth / $threshold"
        startedAtText setText started.html

        visibleExcept(R.id.fundingDepth, R.id.closedAt)
      }
    }

    view setTag this
  }

  class ChanFitAdapter extends BaseAdapter {
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

  def INIT(state: Bundle) = if (app.isAlive) {
    setContentView(R.layout.activity_ln_ops)
    gridView setOnItemClickListener onTap(println)

    gridView.setAdapter(new ChanFitAdapter)
    gridView.setNumColumns(1)

    me setSupportActionBar toolbar
    getSupportActionBar setTitle action_ln_details
    setOnlineChansNum
  } else me exitTo classOf[MainActivity]

  def stateStatusColor(channel: Channel): String = channel.state match {
    case OPEN if isOperational(channel) => s"<font color='#76B041'>$OPEN</font>"
    case NEGOTIATIONS => s"<font color='#FF9900'>$NEGOTIATIONS</font>"
    case OPEN => s"<font color='#76B041'>SHUTDOWN</font>"
    case state => s"<font color='#777777'>$state</font>"
  }

  def connectivityStatusColor(channel: Channel): String = channel.state match {
    case OFFLINE | CLOSING => s"<font color='#777777'><small>disconnected</small></font>"
    case _ => s"<font color='#76B041'><small>online</small></font>"
  }

  def setOnlineChansNum = {
    val num = app.ChannelManager.notClosingOrRefunding.size
    getSupportActionBar setSubtitle app.plurOrZero(onlineChans, num)
  }

  def canDisplay(chanData: ChannelData) = chanData match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case otherwise => true
  }

  def getStat(chanId: BinaryData, direction: Int) = {
    // Direction = 0 = untgoing = lastMast, = 1 = incoming = firstMsat
    val cursor = LNParams.db.select(PaymentTable.selectStatSql, chanId, direction)
    RichCursor(cursor) headTry { case RichCursor(cursor1) => cursor1 getLong direction }
  }
}