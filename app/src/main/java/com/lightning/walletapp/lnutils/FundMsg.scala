package com.lightning.walletapp.lnutils

import fr.acinq.bitcoin._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools.UserId


object FundMsg {
  val FAIL_VERIFY_ERROR = 101
  val FAIL_NOT_VERIFIED_YET = 102

  val FAIL_COULD_NOT_RESERVE = 201
  val FAIL_RESERVE_EXPIRED = 202
  val FAIL_FUNDING_PENDING = 205
  val FAIL_FUNDING_EXISTS = 206
  val FAIL_FUNDING_NONE = 207
  val FAIL_SIGNED_NONE = 208

  val FAIL_INTERNAL_ERROR = 301

  val map = Map(
    FAIL_VERIFY_ERROR -> err_fund_verify_error,
    FAIL_INTERNAL_ERROR -> err_fund_internal_error,
    FAIL_NOT_VERIFIED_YET -> err_fund_not_verified_yet,
    FAIL_COULD_NOT_RESERVE -> err_fund_could_not_reserve,
    FAIL_RESERVE_EXPIRED -> err_fund_reserve_expired,
    FAIL_FUNDING_PENDING -> err_fund_funding_pending,
    FAIL_FUNDING_EXISTS -> err_fund_funding_exists,
    FAIL_FUNDING_NONE -> err_fund_funding_none,
    FAIL_SIGNED_NONE -> err_fund_signed_none
  )
}

trait FundMsg { def userId: UserId }
case class Fail(code: Int, reason: String, userId: UserId = "noUserId") extends FundMsg
case class Start(userId: UserId, fundingAmount: Satoshi, url: String, extra: Option[String] = None) extends FundMsg
case class Started(start: Start, expiry: Long) extends FundMsg { def userId: UserId = start.userId }
case class FundingTxSigned(userId: UserId, txHash: BinaryData, outIndex: Int) extends FundMsg
case class SignFundingTx(userId: UserId, pubkeyScript: BinaryData) extends FundMsg
case class BroadcastFundingTx(userId: UserId, txHash: BinaryData) extends FundMsg
case class FundingTxBroadcasted(userId: UserId, tx: Transaction) extends FundMsg