package com.lightning.walletapp.lnutils

import spray.json._
import scala.concurrent.duration._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.ln.wire._
import com.lightning.walletapp.ln.wire.FundMsg._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import com.lightning.walletapp.ln.Tools.{log, none}
import rx.lang.scala.{Observable => Obs}
import java.util


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
      for (lst <- wrk.listeners) lst.onDisconnect
      wrk.ws.clearListeners
      wrk.ws.disconnect
      worker = None
    }
}

case class Worker(params: Started) { self =>
  val loaded = params.start.url + params.start.toJson.toString.hex
  val ws: WebSocket = (new WebSocketFactory).createSocket(loaded, 7500)
  var listeners: Set[ExternalFunderListener] = Set.empty
  var lastMessage: FundMsg = params.start
  var attemptsLeft: Int = 5

  type JavaList = util.List[String]
  type JavaMap = util.Map[String, JavaList]
  def shouldReconnect: Boolean = lastMessage match {
    case err: Fail => err.code == FAIL_NOT_VERIFIED_YET
    case _: Fail | _: FundingTxBroadcasted => false
    case _ => attemptsLeft > 0
  }

  ws addListener new WebSocketAdapter {
    override def onError(ws: WebSocket, why: WebSocketException) = log(why.getMessage)
    override def onTextMessage(ws: WebSocket, message: String) = for (lst <- listeners) lst onMessage to[FundMsg](message)
    override def onDisconnected(ws: WebSocket, scf: WebSocketFrame, ccf: WebSocketFrame, cbs: Boolean) = onConnectError(ws, null)

    override def onConnected(websocket: WebSocket, headers: JavaMap) = attemptsLeft = 5
    override def onConnectError(ws: WebSocket, reason: WebSocketException) = if (shouldReconnect) {
      Obs.just(attemptsLeft -= 1).delay(5.seconds).foreach(in5Sec => for (lst <- listeners) lst.onAttempt)
      for (listener <- listeners) listener.onOffline
    } else ExternalFunder disconnectWorker self
  }

  listeners += new ExternalFunderListener {
    override def onMessage(msg: FundMsg) = lastMessage = msg
    override def onAttempt = ws.recreate.connectAsynchronously
  }
}

class ExternalFunderListener {
  def onMessage(msg: FundMsg): Unit = none
  def onDisconnect: Unit = none
  def onAttempt: Unit = none
  def onOffline: Unit = none
}