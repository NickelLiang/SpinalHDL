package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

/** Fully connected crossbar, with a hole in its address space so that the decoders instantiate
  * their error slave.
  */
class AxiLite4CrossbarDut(config: AxiLite4Config,
                          val mastersCount: Int,
                          mappings: Seq[SizeMapping],
                          lowLatency: Boolean) extends Component {
  val masters = Vec(slave(AxiLite4(config)), mastersCount)
  val slaves = Vec(master(AxiLite4(config)), mappings.size)

  val factory = AxiLite4CrossbarFactory()
  factory.lowLatency = lowLatency
  factory.addSlaves(mappings.indices.map(i => slaves(i) -> mappings(i)): _*)
  factory.addConnections((0 until mastersCount).map(i => masters(i) -> slaves.indices.map(slaves(_))): _*)
  factory.build()
}

/** Crossbar whose slaves are real [[AxiLite4SlaveFactory]] register files. Their awready depends on
  * wvalid, which is the configuration a crossbar deadlocks on if it waits for aw to be accepted
  * before letting the write data through.
  */
class AxiLite4CrossbarOnSlaveFactoryDut(mastersCount: Int) extends Component {
  val config = AxiLite4Config(addressWidth = 8, dataWidth = 32)
  val masters = Vec(slave(AxiLite4(config)), mastersCount)

  // The crossbar does not translate addresses, so the registers of the slave mapped at 0x80 have
  // to be declared at 0x80 too.
  val slaves = Seq.tabulate(2)(_ => AxiLite4(config))
  val registers = slaves.zipWithIndex.map { case (bus, slaveId) =>
    val factory = new AxiLite4SlaveFactory(bus)
    Seq.tabulate(mastersCount)(masterId =>
      factory.createReadAndWrite(UInt(32 bits), slaveId * 0x80 + masterId * 4) init(0))
  }

  AxiLite4CrossbarFactory()
    .addSlaves(slaves(0) -> SizeMapping(0x00, 0x80), slaves(1) -> SizeMapping(0x80, 0x80))
    .addConnections((0 until mastersCount).map(i => masters(i) -> slaves): _*)
    .build()
}

/** Every shape the factory has to accept, elaborated only. */
class AxiLite4CrossbarShapesDut extends Component {
  val config = AxiLite4Config(addressWidth = 12, dataWidth = 64)

  val iBus = slave(AxiLite4ReadOnly(config))
  val dBus = slave(AxiLite4(config))
  val dmaBus = slave(AxiLite4WriteOnly(config))

  val ram = master(AxiLite4(config))
  val peripherals = master(AxiLite4(config))
  val rom = master(AxiLite4ReadOnly(config))

  AxiLite4CrossbarFactory()
    .addSlaves(
      ram -> SizeMapping(0x000, 0x400),
      peripherals -> SizeMapping(0x400, 0x400),
      rom -> SizeMapping(0x800, 0x400)
    )
    .addConnections(
      iBus -> List(ram, rom),
      dBus -> List(ram, peripherals, rom),
      dmaBus -> List(peripherals) // single master slave, wired without any arbiter
    )
    .addPipelining(dBus) { _ >> _ } { _ >> _ }
    .build()
}

class AxiLite4CrossbarTester extends SpinalAnyFunSuite {

  val config = AxiLite4Config(addressWidth = 12, dataWidth = 32)
  val bytePerWord = config.bytePerWord
  // Three mapped regions of 1 kB, then a 1 kB hole which has to answer DECERR
  val mappings = Seq(SizeMapping(0x000, 0x400), SizeMapping(0x400, 0x400), SizeMapping(0x800, 0x400))
  val holeBase = BigInt(0xC00)
  val regionSize = 0x400

  /** Deterministic content every memory starts with, so that any read can be checked. */
  def initByte(address: BigInt): Byte = ((address * 31 + 7) & 0xFF).toByte

  /** prot is derived from the address, so a slave can check it was not dropped on the way. */
  def protOf(address: BigInt): Int = ((address >> 2) % 8).toInt

  def readWord(mem: mutable.HashMap[BigInt, Byte], address: BigInt): BigInt =
    (0 until bytePerWord).foldLeft(BigInt(0)) { (acc, i) =>
      acc | (BigInt(mem.getOrElseUpdate(address + i, initByte(address + i)) & 0xFF) << (8 * i))
    }

  def writeWord(mem: mutable.HashMap[BigInt, Byte], address: BigInt, data: BigInt, strb: BigInt): Unit =
    for(i <- 0 until bytePerWord if strb.testBit(i)) mem(address + i) = ((data >> (8 * i)) & 0xFF).toByte

  test("crossbar_3m_3s") {
    SimConfig.compile(new AxiLite4CrossbarDut(config, 3, mappings, lowLatency = false))
      .doSim(seed = 42) { dut => runTraffic(dut, 300) }
  }

  test("crossbar_3m_3s_low_latency") {
    SimConfig.compile(new AxiLite4CrossbarDut(config, 3, mappings, lowLatency = true))
      .doSim(seed = 43) { dut => runTraffic(dut, 300) }
  }

  test("crossbar_1m_3s") {
    SimConfig.compile(new AxiLite4CrossbarDut(config, 1, mappings, lowLatency = false))
      .doSim(seed = 44) { dut => runTraffic(dut, 300) }
  }

  test("crossbar_on_slave_factory") {
    SimConfig.compile(new AxiLite4CrossbarOnSlaveFactoryDut(2)).doSim(seed = 45) { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(200000)

      val drivers = (0 until 2).map(i => AxiLite4Driver(dut.masters(i), dut.clockDomain))
      dut.clockDomain.waitSampling(10)

      val threads = (0 until 2).map { masterId =>
        fork {
          for(round <- 0 until 8; slaveId <- 0 until 2) {
            val address = slaveId * 0x80 + masterId * 4
            val value = BigInt(masterId * 0x10000 + slaveId * 0x100 + round)
            drivers(masterId).write(address, value)
            val readBack = drivers(masterId).read(address)
            assert(readBack == value, f"master $masterId read $readBack%#x instead of $value%#x at $address%#x")
          }
        }
      }
      threads.foreach(_.join())
    }
  }

  test("crossbar_shapes_generation") {
    SpinalVerilog(new AxiLite4CrossbarShapesDut)
  }

  test("crossbar_config_mismatch_is_rejected") {
    assertThrows[Throwable] {
      SpinalVerilog(new Component {
        val m = slave(AxiLite4(config))
        val s = master(AxiLite4(config.copy(dataWidth = 64)))
        AxiLite4CrossbarFactory().addSlave(s, SizeMapping(0x000, 0x1000)).addConnection(m, List(s)).build()
      })
    }
  }

  test("crossbar_overlapping_mappings_are_rejected") {
    assertThrows[Throwable] {
      val overlapping = Seq(SizeMapping(0x000, 0x400), SizeMapping(0x200, 0x400))
      SpinalVerilog(new AxiLite4CrossbarDut(config, 1, overlapping, lowLatency = false))
    }
  }

  def runTraffic(dut: AxiLite4CrossbarDut, transactionsPerMaster: Int): Unit = {
    dut.clockDomain.forkStimulus(10)
    SimTimeout(1000000)

    val slaveMems = Seq.fill(mappings.size)(mutable.HashMap[BigInt, Byte]())
    for(i <- mappings.indices) new SlaveAgent(dut.slaves(i), dut.clockDomain, mappings(i).base, slaveMems(i))

    val agents = (0 until dut.mastersCount).map(i =>
      new MasterAgent(dut.masters(i), dut.clockDomain, i, dut.mastersCount, mappings.size))

    dut.clockDomain.waitSampling(10)
    agents.foreach(_.start(transactionsPerMaster))

    dut.clockDomain.waitSamplingWhere(agents.forall(_.done))
    dut.clockDomain.waitSampling(50)

    for(a <- agents) {
      assert(a.readCount + a.writeCount == transactionsPerMaster,
        s"master ${a.id} completed ${a.readCount + a.writeCount} out of $transactionsPerMaster transactions")
      assert(a.readCount > 0 && a.writeCount > 0 && a.decErrCount > 0,
        s"master ${a.id} did not cover reads(${a.readCount}) writes(${a.writeCount}) decode errors(${a.decErrCount})")
    }
  }

  /** Slave model backed by a byte addressed memory. */
  class SlaveAgent(bus: AxiLite4, cd: ClockDomain, base: BigInt, mem: mutable.HashMap[BigInt, Byte]) {
    def checkAddress(address: BigInt, prot: Int, kind: String): Unit = {
      assert(address >= base && address < base + regionSize,
        f"the slave mapped at $base%#x got a $kind at $address%#x, outside of its mapping")
      assert(prot == protOf(address),
        f"$kind prot altered on the way to $address%#x: $prot instead of ${protOf(address)}")
    }

    val arQueue = mutable.Queue[BigInt]()
    val awQueue = mutable.Queue[BigInt]()
    val wQueue = mutable.Queue[(BigInt, BigInt)]()

    StreamReadyRandomizer(bus.ar, cd)
    StreamMonitor(bus.ar, cd) { ar =>
      checkAddress(ar.addr.toBigInt, ar.prot.toInt, "read")
      arQueue += ar.addr.toBigInt
    }
    StreamDriver(bus.r, cd) { r =>
      if(arQueue.nonEmpty) {
        r.data #= readWord(mem, arQueue.dequeue())
        r.resp #= 0
        true
      } else false
    }

    StreamReadyRandomizer(bus.aw, cd)
    StreamMonitor(bus.aw, cd) { aw =>
      checkAddress(aw.addr.toBigInt, aw.prot.toInt, "write")
      awQueue += aw.addr.toBigInt
    }
    StreamReadyRandomizer(bus.w, cd)
    StreamMonitor(bus.w, cd) { w => wQueue += ((w.data.toBigInt, w.strb.toBigInt)) }
    StreamDriver(bus.b, cd) { b =>
      if(awQueue.nonEmpty && wQueue.nonEmpty) {
        val (data, strb) = wQueue.dequeue()
        writeWord(mem, awQueue.dequeue(), data, strb)
        b.resp #= 0
        true
      } else false
    }
  }

  /** Master model keeping several transactions in flight, which the stock AxiLite4Master cannot do.
    * Each master owns a disjoint set of words, so a response delivered to the wrong master shows up
    * as a read mismatch, and checking the responses against a queue also checks their ordering.
    */
  class MasterAgent(bus: AxiLite4, cd: ClockDomain, val id: Int, mastersCount: Int, slavesCount: Int) {
    val refMem = mutable.HashMap[BigInt, Byte]()
    val inFlight = mutable.HashSet[BigInt]()

    val arQueue = mutable.Queue[BigInt]()
    val awQueue = mutable.Queue[BigInt]()
    val wQueue = mutable.Queue[(BigInt, BigInt)]()
    /** (address, expected data or None when the data is don't care, expected resp) */
    val rExpect = mutable.Queue[(BigInt, Option[BigInt], Int)]()
    val bExpect = mutable.Queue[(BigInt, Int)]()

    var readCount = 0
    var writeCount = 0
    var decErrCount = 0
    var issued = 0
    var target = 0

    def done: Boolean = issued == target && rExpect.isEmpty && bExpect.isEmpty

    /** An address owned by this master: word indexes are interleaved between the masters. */
    def ownedAddress(): BigInt = {
      val slave = Random.nextInt(slavesCount)
      val word = Random.nextInt(regionSize / bytePerWord / mastersCount) * mastersCount + id
      mappings(slave).base + word * bytePerWord
    }

    def holeAddress(): BigInt = holeBase + Random.nextInt(regionSize / bytePerWord) * bytePerWord

    def start(transactions: Int): Unit = {
      target = transactions
      fork {
        while(issued < target) {
          val decodeError = Random.nextInt(10) == 0
          val address = if(decodeError) holeAddress() else ownedAddress()
          // The read and the write path of the crossbar are independent, so a read and a write of
          // the same address must not be in flight at the same time: their order is not defined.
          if(decodeError || !inFlight.contains(address)) {
            if(decodeError) decErrCount += 1 else inFlight += address
            val resp = if(decodeError) 3 else 0
            if(Random.nextBoolean()) {
              rExpect += ((address, if(decodeError) None else Some(readWord(refMem, address)), resp))
              arQueue += address
            } else {
              val data = BigInt(config.dataWidth, Random)
              val strb = BigInt(bytePerWord, Random)
              if(!decodeError) {
                writeWord(refMem, address, data, strb)
              }
              bExpect += ((address, resp))
              awQueue += address
              wQueue += ((data, strb))
            }
            issued += 1
          }
          cd.waitSampling(Random.nextInt(3))
        }
      }
    }

    StreamDriver(bus.ar, cd) { ar =>
      if(arQueue.nonEmpty && Random.nextInt(4) != 0) {
        val address = arQueue.dequeue()
        ar.addr #= address
        ar.prot #= protOf(address)
        true
      } else false
    }
    StreamReadyRandomizer(bus.r, cd)
    StreamMonitor(bus.r, cd) { r =>
      assert(rExpect.nonEmpty, s"master $id got an unexpected read response")
      val (address, data, resp) = rExpect.dequeue()
      assert(r.resp.toInt == resp, f"master $id read at $address%#x: resp ${r.resp.toInt} instead of $resp")
      data.foreach(expected => assert(r.data.toBigInt == expected,
        f"master $id read at $address%#x: got ${r.data.toBigInt}%#x instead of $expected%#x"))
      inFlight -= address
      readCount += 1
    }

    // aw and w are driven from independent queues, so that w regularly leads aw
    StreamDriver(bus.aw, cd) { aw =>
      if(awQueue.nonEmpty && Random.nextInt(4) != 0) {
        val address = awQueue.dequeue()
        aw.addr #= address
        aw.prot #= protOf(address)
        true
      } else false
    }
    StreamDriver(bus.w, cd) { w =>
      if(wQueue.nonEmpty && Random.nextInt(4) != 0) {
        val (data, strb) = wQueue.dequeue()
        w.data #= data
        w.strb #= strb
        true
      } else false
    }
    StreamReadyRandomizer(bus.b, cd)
    StreamMonitor(bus.b, cd) { b =>
      assert(bExpect.nonEmpty, s"master $id got an unexpected write response")
      val (address, resp) = bExpect.dequeue()
      assert(b.resp.toInt == resp, f"master $id write at $address%#x: resp ${b.resp.toInt} instead of $resp")
      inFlight -= address
      writeCount += 1
    }
  }
}
