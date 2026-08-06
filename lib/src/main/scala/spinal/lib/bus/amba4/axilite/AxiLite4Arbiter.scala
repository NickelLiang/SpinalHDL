package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

/**
  * Share one AxiLite4 slave between several masters, with a round robin arbitration on ar.
  *
  * Where the AXI4 arbiter extends the ID to route the responses back, AXI4-Lite has no ID and
  * therefore a single implicit one: the slave answers in the order it accepted the commands, so a
  * FIFO of the arbitration decisions routes r exactly. Nothing checks that at run time, and a slave
  * which answers out of order would have its responses delivered to the wrong master.
  *
  * @param outputConfig    Axi Lite configuration class, shared by the inputs and the output
  * @param inputsCount     number of masters to arbitrate
  * @param routeBufferSize how many commands can be issued before their response came back
  */
case class AxiLite4ReadOnlyArbiter(outputConfig: AxiLite4Config,
                                   inputsCount: Int,
                                   routeBufferSize: Int = 4) extends Component {
  assert(routeBufferSize >= 1)

  val io = new Bundle {
    val inputs = Vec(slave(AxiLite4ReadOnly(outputConfig)), inputsCount)
    val output = master(AxiLite4ReadOnly(outputConfig))
  }

  // Route readCmd
  val cmdArbiter = StreamArbiterFactory().roundRobin.build(AxiLite4Ax(outputConfig), inputsCount)
  (cmdArbiter.io.inputs, io.inputs.map(_.readCmd)).zipped.map(_ <> _)
  val (cmdOutputFork, cmdRouteFork) = StreamFork2(cmdArbiter.io.output)
  io.output.readCmd << cmdOutputFork

  // Route readRsp. The fork above also throttles the commands to routeBufferSize outstanding.
  val routeBuffer = cmdRouteFork.translateWith(cmdArbiter.io.chosen).queueLowLatency(routeBufferSize)
  val readRspIndex = routeBuffer.payload
  for((input, idx) <- io.inputs.zipWithIndex){
    input.readRsp.valid := io.output.readRsp.valid && routeBuffer.valid && readRspIndex === idx
    input.readRsp.payload := io.output.readRsp.payload
  }
  io.output.readRsp.ready := routeBuffer.valid && io.inputs(readRspIndex).readRsp.ready
  routeBuffer.ready := io.output.readRsp.fire
}

/**
  * Share one AxiLite4 slave between several masters, with a round robin arbitration on aw.
  *
  * Two FIFOs of arbitration decisions, see [[AxiLite4ReadOnlyArbiter]] about why a FIFO is enough:
  * one popped by w, which keeps the write data of two masters from interleaving into the same
  * slave, and one popped by b. A master which delays its w beat therefore blocks the writes of the
  * other masters toward that slave.
  *
  * @param outputConfig    Axi Lite configuration class, shared by the inputs and the output
  * @param inputsCount     number of masters to arbitrate
  * @param routeBufferSize how many commands can be issued before their response came back
  */
case class AxiLite4WriteOnlyArbiter(outputConfig: AxiLite4Config,
                                    inputsCount: Int,
                                    routeBufferSize: Int = 4) extends Component {
  assert(routeBufferSize >= 1)

  val io = new Bundle {
    val inputs = Vec(slave(AxiLite4WriteOnly(outputConfig)), inputsCount)
    val output = master(AxiLite4WriteOnly(outputConfig))
  }

  // Route writeCmd
  val cmdArbiter = StreamArbiterFactory().roundRobin.build(AxiLite4Ax(outputConfig), inputsCount)
  (cmdArbiter.io.inputs, io.inputs.map(_.writeCmd)).zipped.map(_ <> _)
  val (cmdOutputFork, cmdDataFork, cmdRspFork) = StreamFork3(cmdArbiter.io.output)
  io.output.writeCmd << cmdOutputFork

  // Route writeData
  val dataRouteBuffer = cmdDataFork.translateWith(cmdArbiter.io.chosen).queueLowLatency(routeBufferSize)
  val routeDataInput = io.inputs(dataRouteBuffer.payload).writeData
  io.output.writeData.valid := dataRouteBuffer.valid && routeDataInput.valid
  io.output.writeData.payload := routeDataInput.payload
  io.inputs.zipWithIndex.foreach { case (input, idx) =>
    input.writeData.ready := dataRouteBuffer.valid && io.output.writeData.ready && dataRouteBuffer.payload === idx
  }
  dataRouteBuffer.ready := io.output.writeData.fire

  // Route writeRsp
  val rspRouteBuffer = cmdRspFork.translateWith(cmdArbiter.io.chosen).queueLowLatency(routeBufferSize)
  val writeRspIndex = rspRouteBuffer.payload
  for((input, idx) <- io.inputs.zipWithIndex){
    input.writeRsp.valid := io.output.writeRsp.valid && rspRouteBuffer.valid && writeRspIndex === idx
    input.writeRsp.payload := io.output.writeRsp.payload
  }
  io.output.writeRsp.ready := rspRouteBuffer.valid && io.inputs(writeRspIndex).writeRsp.ready
  rspRouteBuffer.ready := io.output.writeRsp.fire
}
