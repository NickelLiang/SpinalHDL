package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib.slave

/**
  * Answers every write transaction with a DECERR response. Instantiated by
  * [[AxiLite4WriteOnlyDecoder]] to cover the address space left unmapped by its decodings.
  *
  * @param axiConfig Axi Lite configuration class
  */
case class AxiLite4WriteOnlyErrorSlave(axiConfig: AxiLite4Config) extends Component {
  val io = new Bundle {
    val axi = slave(AxiLite4WriteOnly(axiConfig))
  }

  val consumeData = RegInit(False)
  val sendRsp     = RegInit(False)

  io.axi.writeCmd.ready := !(consumeData || sendRsp)
  when(io.axi.writeCmd.fire) {
    consumeData := True
  }

  io.axi.writeData.ready := consumeData
  when(io.axi.writeData.fire) {
    consumeData := False
    sendRsp := True
  }

  io.axi.writeRsp.valid := sendRsp
  io.axi.writeRsp.setDECERR()
  when(io.axi.writeRsp.fire) {
    sendRsp := False
  }
}

/**
  * Answers every read transaction with a DECERR response. Instantiated by
  * [[AxiLite4ReadOnlyDecoder]] to cover the address space left unmapped by its decodings.
  *
  * @param axiConfig Axi Lite configuration class
  */
case class AxiLite4ReadOnlyErrorSlave(axiConfig: AxiLite4Config) extends Component {
  val io = new Bundle {
    val axi = slave(AxiLite4ReadOnly(axiConfig))
  }

  val sendRsp = RegInit(False)

  io.axi.readCmd.ready := !sendRsp
  when(io.axi.readCmd.fire) {
    sendRsp := True
  }

  io.axi.readRsp.valid := sendRsp
  io.axi.readRsp.data.assignDontCare()
  io.axi.readRsp.setDECERR()
  when(io.axi.readRsp.fire) {
    sendRsp := False
  }
}
