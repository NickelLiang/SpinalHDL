package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

// Mirrors spinal.lib.bus.amba4.axi.Axi4Arbiter, with route FIFOs in place of its id extension.

/**
  * Share one AxiLite4 slave between several masters, using a round robin arbitration on the
  * command channel.
  *
  * The AXI4 arbiters route the responses back by extending the transaction ID with the index of
  * the chosen master. AXI4-Lite has no ID, but for the same reason it also has a single implicit
  * ID: a slave has to answer in the order it accepted the commands. So the arbitration decisions
  * are pushed into a FIFO instead, and popped in the same order by the response channel.
  *
  * @note This is the whole correctness argument, and nothing checks it at run time: a slave which
  *       answers out of the order it accepted the commands is not AXI4-Lite compliant, and would
  *       have its responses silently delivered to the wrong master.
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
  * Share one AxiLite4 slave between several masters, using a round robin arbitration on the
  * command channel.
  *
  * Two FIFOs of arbitration decisions are kept: one consumed by the w channel, so that the write
  * data of two masters is never interleaved into the same slave, and one consumed by the b channel
  * to route the responses back. See [[AxiLite4ReadOnlyArbiter]] about why a FIFO is enough, and
  * about what a non compliant slave would break.
  *
  * @note A master which wins the arbitration on aw and then delays its w beat blocks the writes of
  *       the other masters toward that slave, as the w channel has to stay bound to its aw.
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
  io.inputs.zipWithIndex.foreach{ case(input, idx) =>
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
