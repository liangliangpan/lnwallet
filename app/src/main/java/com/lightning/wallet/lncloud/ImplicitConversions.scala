package com.lightning.wallet.lncloud

import org.bitcoinj.core.Utils._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.Utils.app
import language.implicitConversions
import java.math.BigInteger
import android.text.Html


object ImplicitConversions {
  implicit def bitcoinLibScript2bitcoinjScript(bitcoinLibScript: ScriptEltSeq): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(fr.acinq.bitcoin.Script write bitcoinLibScript, System.currentTimeMillis)

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction read bitcoinjTx.unsafeBitcoinSerialize

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction write bitcoinLibTx)

  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)
}

class StringOps(source: String) {
  def bigInteger = new BigInteger(source)
  def hex = HEX.encode(source getBytes "UTF-8")
  def noCommas = source.replace(",", "")
  def html = Html fromHtml source
}