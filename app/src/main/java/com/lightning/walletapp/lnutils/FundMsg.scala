package com.lightning.walletapp.lnutils

import com.lightning.walletapp.ln.Tools.UserId
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}


object FundMsg {
  val FAIL_VERIFY_ERROR = 101
  val FAIL_NOT_VERIFIED_YET = 102
  val FAIL_INTERNAL_ERROR = 301

  val FAIL_COULD_NOT_RESERVE = 201
  val FAIL_RESERVE_EXPIRED = 202
  val FAIL_AMOUNT_TOO_LARGE = 203
  val FAIL_AMOUNT_TOO_SMALL = 204
  val FAIL_FUNDING_PENDING = 205
  val FAIL_FUNDING_EXISTS = 206
  val FAIL_FUNDING_NONE = 207
  val FAIL_SIGNED_NONE = 208
}

trait FundMsg { def userId: UserId }
case class Fail(code: Int, reason: String, userId: UserId = "noUserId") extends FundMsg
case class Start(userId: UserId, fundingAmount: Satoshi, url: String, extra: Option[String] = None) extends FundMsg
case class Started(start: Start, expiry: Long) extends FundMsg { def userId: UserId = start.userId }
case class FundingTxSigned(userId: UserId, txHash: BinaryData, outIndex: Int) extends FundMsg
case class SignFundingTx(userId: UserId, pubkeyScript: BinaryData) extends FundMsg
case class BroadcastFundingTx(userId: UserId, txHash: BinaryData) extends FundMsg
case class FundingTxBroadcasted(userId: UserId, tx: Transaction) extends FundMsg