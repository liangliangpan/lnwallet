package com.lightning.walletapp.lnutils

import spray.json._
import com.neovisionaries.ws.client._
import com.lightning.walletapp.lnutils.JsonHttpUtils._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.Tools.{none, runAnd}


object ExternalFunder {
  var worker = Option.empty[Worker]
  def safeDisconnectWorker: Unit = {
    for (w <- worker) w.ws.clearListeners
    for (w <- worker) w.ws.disconnect
    worker = None
  }

  def setWorker(parameters: Started) = {
    if (worker.isDefined) safeDisconnectWorker
    val newConnection = new Worker(parameters)
    newConnection.ws.connectAsynchronously
    worker = Some(newConnection)
  }
}

class Worker(parameters: Started) { me =>
  val ws = (new WebSocketFactory).createSocket(parameters.start.url, 7500)
  var listeners: Set[ExternalFunderListener] = Set.empty
  var lastMessage: FundMsg = parameters

  def update(message: String) = {
    lastMessage = to[FundMsg](message)
    for (lst <- listeners) lst onOffline me
  }

  ws addListener new WebSocketAdapter {
    override def onTextMessage(ws: WebSocket, message: String) = try update(message) catch none
    override def onConnectError(ws: WebSocket, why: WebSocketException) = for (lst <- listeners) lst onOffline me
    override def onDisconnected(ws: WebSocket, scf: WebSocketFrame, ccf: WebSocketFrame, cbs: Boolean) = onConnectError(ws, null)
  }
}

class ExternalFunderListener {
  def onMessage(message: FundMsg): Unit = none
  def onOffline(worker: Worker): Unit = none
}