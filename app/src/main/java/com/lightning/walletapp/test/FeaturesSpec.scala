package com.lightning.walletapp.test

import java.nio.ByteOrder
import fr.acinq.bitcoin.{BinaryData, Protocol}


class FeaturesSpec {
  import com.lightning.walletapp.ln.Features._
  
  def allTests = {

    {
      println("'data_loss_protect' feature")
      assert(dataLossProtect(BinaryData("02"))) // optional
      assert(!dataLossProtect(BinaryData("00")))
      assert(dataLossProtect(BinaryData("81"))) // mandatory
    }

    {
      println("features compatibility")
      assert(areSupported(BinaryData(Protocol.writeUInt64(1L << OPTION_DATA_LOSS_PROTECT_OPTIONAL, ByteOrder.BIG_ENDIAN))))
      assert(!areSupported(BinaryData("14")))
      assert(!areSupported(BinaryData("0141")))
    }

  }
}
