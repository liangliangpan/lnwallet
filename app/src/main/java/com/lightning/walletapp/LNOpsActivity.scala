package com.lightning.walletapp

import com.lightning.walletapp.Utils._
import android.os.Bundle
import com.lightning.walletapp.R.string._
import android.support.v7.widget.Toolbar
import android.view.{View, ViewGroup}
import android.widget.{BaseAdapter, GridView}
import com.lightning.walletapp.ln.{ChannelData, RefundingData}


class LNOpsActivity extends TimerActivity { me =>
  val onlineChans = app.getResources getStringArray R.array.ln_chan_online
  lazy val gridView = findViewById(R.id.gridView).asInstanceOf[GridView]
  lazy val toolbar = findViewById(R.id.toolbar).asInstanceOf[Toolbar]

  class ChanFitAdapter extends BaseAdapter {
    val localChanCache = {
      val x = for (chan <- app.ChannelManager.all if me canDisplay chan.data) yield chan
      x ++ x ++ x ++ x ++ x ++ x ++ x ++ x ++ x
    }

    def getCount = localChanCache.size
    def getItem(position: Int) = localChanCache(position)
    def getItemId(position: Int) = position

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val card = getLayoutInflater.inflate(R.layout.chan_card, null)
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
  } else me exitTo classOf[MainActivity]

  def canDisplay(chanData: ChannelData) = chanData match {
    case ref: RefundingData => ref.remoteLatestPoint.isDefined
    case otherwise => true
  }
}