package com.lightning.walletapp

import android.view._
import android.widget._
import android.support.v4.app._
import com.lightning.walletapp.ln._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.lnutils._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.StartNodeView._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.lnutils.olympus.OlympusWrap._
import com.lightning.walletapp.helper.ThrottledWork
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import fr.acinq.bitcoin.Satoshi
import android.os.Bundle
import java.util.Date


class LNStartActivity extends ScanActivity { me =>
  lazy val slidingFragmentAdapter = new FragmentStatePagerAdapter(getSupportFragmentManager) {
    def getItem(currentFragmentPos: Int) = if (0 == currentFragmentPos) new FragLNStart else new FragScan
    def getCount = 2
  }

  override def onBackPressed = {
    val isScannerOpen = 1 == walletPager.getCurrentItem
    if (isScannerOpen) walletPager.setCurrentItem(0, true)
    else super.onBackPressed
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionScan) walletPager.setCurrentItem(1, true)
  }

  override def onCreateOptionsMenu(menu: Menu) = runAnd(true) {
    // Called after FragLNStart sets its toolbar as actionbar
    getMenuInflater.inflate(R.menu.ln_start, menu)
    FragLNStart.fragment.setupSearch(menu)
  }

  def INIT(s: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_double_pager
    walletPager setAdapter slidingFragmentAdapter
  } else me exitTo classOf[MainActivity]

  def checkTransData = {
    returnToBase(view = null)
    app.TransData.value match {
      case strt: Started => FragLNStart.fragment.spawnExternalFunder(strt)
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case _: PaymentRequest => me exitTo classOf[WalletActivity]
      case _: BitcoinURI => me exitTo classOf[WalletActivity]
      case _: Address => me exitTo classOf[WalletActivity]
      case _ => app.TransData.value = null
    }
  }
}

object FragLNStart {
  var fragment: FragLNStart = _
}

class FragLNStart extends Fragment with SearchBar with HumanTimeDisplay { me =>
  override def onDestroy = wrap(super.onDestroy)(ExternalFunder.worker foreach ExternalFunder.disconnectWorker)
  override def onCreateView(inf: LayoutInflater, vg: ViewGroup, bn: Bundle) = inf.inflate(R.layout.frag_ln_start, vg, false)
  lazy val host: LNStartActivity = me.getActivity.asInstanceOf[LNStartActivity]

  def humanWorker(wrk: Worker, base: Int) = {
    val expiry = me time new Date(wrk.params.expiry)
    val amount = denom withSign wrk.params.start.fundingAmount
    host.getString(base).format(wrk.params.start.host, amount, expiry)
  }

  var spawnExternalFunder: Started => Unit = none
  override def onViewCreated(view: View, state: Bundle) = {
    val lnStartNodesList = view.findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
    val externalFundInfo = view.findViewById(R.id.externalFundInfo).asInstanceOf[TextView]
    val externalFundCancel = view.findViewById(R.id.externalFundCancel).asInstanceOf[Button]
    val externalFundWrap = view.findViewById(R.id.externalFundWrap).asInstanceOf[LinearLayout]
    val toolbar = view.findViewById(R.id.toolbar).asInstanceOf[android.support.v7.widget.Toolbar]
    var nodes = Vector.empty[StartNodeView]

    val adapter = new BaseAdapter {
      def getView(pos: Int, savedView: View, par: ViewGroup) = {
        val slot = host.getLayoutInflater.inflate(R.layout.frag_single_line, null)
        val textLine = slot.findViewById(R.id.textLine).asInstanceOf[TextView]
        textLine setText getItem(pos).asString(nodeView, "\u0020").html
        slot
      }

      def getItem(position: Int) = nodes(position)
      def getItemId(position: Int) = position
      def getCount = nodes.size
    }

    def onSelect(pos: Int) = {
      app.TransData.value = adapter getItem pos
      host goTo classOf[LNStartFundActivity]
    }

    val err2String = Map(
      FAIL_VERIFY_ERROR -> err_fund_verify_error,
      FAIL_NOT_VERIFIED_YET -> err_fund_not_verified_yet,
      FAIL_INTERNAL_ERROR -> err_fund_internal_error,

      FAIL_RESERVE_FAILED -> err_fund_reserve_failed,
      FAIL_RESERVE_EXPIRED -> err_fund_reserve_expired,
      FAIL_FUNDING_PENDING -> err_fund_funding_pending,
      FAIL_FUNDING_EXISTS -> err_fund_funding_exists,
      FAIL_PUBLISH_ERROR -> err_publish_error
    )

    spawnExternalFunder = started => {
      val freshWorker = Worker(started)
      externalFundWrap setVisibility View.VISIBLE
      externalFundInfo setText humanWorker(freshWorker, ex_fund_connecting).html
      val disconnect = host.onButtonTap(ExternalFunder disconnectWorker freshWorker)
      externalFundCancel setOnClickListener disconnect

      freshWorker.listeners += new ExternalFunderListener {
        override def onMessage(msg: FundMsg) = host.UITask(externalFundInfo setText humanWorker(freshWorker, ex_fund_connected).html).run
        override def onOffline = host.UITask(externalFundInfo setText humanWorker(freshWorker, ex_fund_connecting).html).run
        override def onDisconnect = host.UITask(externalFundWrap setVisibility View.GONE).run
      }

      freshWorker.listeners += new ExternalFunderListener {
        override def onMessage(message: FundMsg) = message match {
          case fail: Fail if err2String contains fail.code => host onFail getString(err2String apply fail.code)
          case unexpected: Fail => host onFail getString(err2String apply FAIL_INTERNAL_ERROR)
          case _ =>
        }
      }

      freshWorker.listeners += new ExternalFunderListener {
        override def onMessage(message: FundMsg) = message match {
          case fail: Fail if fail.code != FAIL_NOT_VERIFIED_YET => disconnect onClick null
          case fundingTxBroadcasted: FundingTxBroadcasted => disconnect onClick null
          case _ =>
        }
      }

      // Try to connect and clear TransData
      ExternalFunder setWorker freshWorker
      app.TransData.value = null
    }

    new ThrottledWork[String, AnnounceChansNumVec] {
      def work(userQuery: String) = findNodes(userQuery)
      def error(error: Throwable) = Tools errlog error
      me.react = addWork

      def process(userQuery: String, res: AnnounceChansNumVec) = {
        nodes = for (announce <- res) yield RemoteNodeView(announce)
        host.UITask(adapter.notifyDataSetChanged).run
      }
    }

    FragLNStart.fragment = me
    host.setSupportActionBar(toolbar)
    host.getSupportActionBar.setTitle(action_ln_open)
    host.getSupportActionBar.setSubtitle(ln_status_peer)
    lnStartNodesList.setOnItemClickListener(host onTap onSelect)
    lnStartNodesList.setAdapter(adapter)
    host.checkTransData
    react(new String)
  }
}

// DISPLAYING NODES ON UI

object StartNodeView {
  lazy val nodeView = app getString ln_ops_start_node_view
  lazy val nodeFundView = app getString ln_ops_start_fund_node_view
  lazy val chansNumber = app.getResources getStringArray R.array.ln_ops_start_node_channels
}

sealed trait StartNodeView { def asString(base: String, separator: String): String }
// This invariant comes from nodes recommended by dev but also when a node QR is scanned
case class HardcodedNodeView(ann: NodeAnnouncement, tip: String) extends StartNodeView {

  def asString(base: String, separator: String) = {
    val key = humanNode(ann.nodeId.toString, separator)
    base.format(ann.alias, tip, key)
  }
}

// This invariant comes as a search result from Olympus server queries
case class RemoteNodeView(acn: AnnounceChansNum) extends StartNodeView {

  def asString(base: String, separator: String) = {
    val channelAnnouncement \ channelConnections = acn
    val humanConnects = app.plurOrZero(chansNumber, channelConnections)
    val key = humanNode(channelAnnouncement.nodeId.toString, separator)
    base.format(channelAnnouncement.alias, humanConnects, key)
  }
}