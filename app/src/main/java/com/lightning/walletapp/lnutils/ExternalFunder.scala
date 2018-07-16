package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import rx.lang.scala.{Observable => Obs}
import com.lightning.walletapp.ln.Tools


object ExternalFunder {
  var worker = Option.empty[Worker]
  def setWorker(newWorker: Worker) = {
    // This is guaranteed to disconnect it
    for (old <- worker) disconnectWorker(old)
    newWorker.ws.connectAsynchronously
    worker = Some(newWorker)
  }

  def disconnectWorker(oldWorker: Worker) =
    for (wrk <- worker if wrk.params == oldWorker.params) {
      // Only disconnect if old worker is also a current worker
      wrk.ws.clearListeners
      wrk.ws.disconnect
    }

  val map = Map(
    FAIL_VERIFY_ERROR -> err_fund_verify_error,
    FAIL_NOT_VERIFIED_YET-> err_fund_not_verified_yet,
    FAIL_INTERNAL_ERROR -> err_fund_internal_error,

    FAIL_RESERVE_FAILED -> err_fund_reserve_failed,
    FAIL_RESERVE_EXPIRED -> err_fund_reserve_expired,
    FAIL_FUNDING_PENDING -> err_fund_funding_pending,
    FAIL_FUNDING_EXISTS -> err_fund_funding_exists,
    FAIL_PUBLISH_ERROR -> err_publish_error
  )
}

case class Worker(params: Started, attemptsLeft: Int = 5) { me =>
  val url = "ws://" + params.start.url + "/" + params.start.toJson.toString.hex
  val ws: WebSocket = (new WebSocketFactory).createSocket(url, 7500)
  var listeners: Set[ExternalFunderListener] = Set.empty
  var lastMessage: FundMsg = params.start

  def shouldReconnect = lastMessage match {
    case err: Fail => err.code == FAIL_NOT_VERIFIED_YET
    case _: Fail | _: FundingTxBroadcasted => false
    case _ => attemptsLeft > 0
  }

  ws addListener new WebSocketAdapter {
    override def onError(ws: WebSocket, why: WebSocketException) = Tools errlog why
    override def onTextMessage(ws: WebSocket, message: String) = for (lst <- listeners) lst onMessage to[FundMsg](message)
    override def onDisconnected(ws: WebSocket, scf: WebSocketFrame, ccf: WebSocketFrame, cbs: Boolean) = onConnectError(ws, null)

    override def onConnectError(ws: WebSocket, reason: WebSocketException) = if (shouldReconnect) {
      Obs.just(null).delay(5.seconds).foreach(_ => for (lst <- listeners) lst onReconnect me, Tools.none)
      for (listener <- listeners) listener onOffline me
    } else for (lst <- listeners) lst onCancel me
  }

  listeners += new ExternalFunderListener {
    override def onMessage(message: FundMsg) = lastMessage = message
    override def onCancel(worker: Worker) = ExternalFunder disconnectWorker me

    override def onReconnect(worker: Worker) = {
      val worker1 = Worker(params, attemptsLeft - 1)
      ExternalFunder setWorker worker1
    }
  }
}

class ExternalFunderListener {
  def onMessage(message: FundMsg): Unit = Tools.none
  def onReconnect(worker: Worker): Unit = Tools.none
  def onOffline(worker: Worker): Unit = Tools.none
  def onCancel(worker: Worker): Unit = Tools.none
}