package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

case class AxiLite4CrossbarSlaveConnection(master: AxiLite4Bus)

case class AxiLite4CrossbarSlaveConfig(mapping: SizeMapping){
  val connections = ArrayBuffer[AxiLite4CrossbarSlaveConnection]()
}

/**
  * Build an AXI4-Lite interconnect out of one [[AxiLite4ReadOnlyDecoder]] /
  * [[AxiLite4WriteOnlyDecoder]] per master and one [[AxiLite4ReadOnlyArbiter]] /
  * [[AxiLite4WriteOnlyArbiter]] per slave. Slaves reached by a single master are wired directly,
  * without any arbiter; `addPipelining` on that slave is still applied.
  *
  * The API mirrors `spinal.lib.bus.amba4.axi.Axi4CrossbarFactory`, so an AXI4 interconnect can be
  * turned into an AXI4-Lite one without rewriting its description. Addresses are not translated:
  * a slave mapped at 0x1000 sees the addresses of the master, not offsets from its own base.
  *
  * Throughput, all inherited from the AXI4 interconnect this mirrors:
  *  - a master can only have outstanding transactions toward one slave at a time, so alternating
  *    between two slaves costs a full round trip (see [[AxiLite4ReadOnlyDecoder]]);
  *  - `routeBufferSize` (4) is lower than `pendingMax` (7), so by default the arbiters bound the
  *    outstanding transactions before the decoders do;
  *  - the response path is fully combinatorial. `lowLatency` and `arValidPipe`/`awValidPipe` only
  *    cut the command path, `addPipelining` is the way to register the responses too. The decoder
  *    records the destination one cycle after the command is accepted, so a slave must not respond
  *    in that same cycle.
  *
  * @example {{{
  *   AxiLite4CrossbarFactory()
  *     .addSlaves(
  *       ram.io.axi  -> SizeMapping(0x00000, 16 kB),
  *       uart.io.axi -> SizeMapping(0x10000,  4 kB)
  *     )
  *     .addConnections(
  *       core.io.dBus -> List(ram.io.axi, uart.io.axi),
  *       core.io.iBus -> List(ram.io.axi)
  *     )
  *     .build()
  * }}}
  */
case class AxiLite4CrossbarFactory(){
  val slavesConfigs = mutable.LinkedHashMap[AxiLite4Bus, AxiLite4CrossbarSlaveConfig]()
  val axiLite4ToReadWriteOnly = mutable.HashMap[AxiLite4, Seq[AxiLite4Bus]]()
  val readOnlyBridger = mutable.HashMap[AxiLite4ReadOnly, (AxiLite4ReadOnly, AxiLite4ReadOnly) => Unit]()
  val writeOnlyBridger = mutable.HashMap[AxiLite4WriteOnly, (AxiLite4WriteOnly, AxiLite4WriteOnly) => Unit]()
  val masters = ArrayBuffer[AxiLite4Bus]()

  /** Skip the register which cuts the combinatorial path between a decoder and an arbiter. */
  var lowLatency = false
  /** Maximum number of outstanding transactions per master, see [[AxiLite4ReadOnlyDecoder]]. */
  var pendingMax = 7
  /** Maximum number of outstanding transactions per slave, see [[AxiLite4ReadOnlyArbiter]]. */
  var routeBufferSize = 4

  def decoderToArbiterLink(bus: AxiLite4ReadOnly) = if(!lowLatency) bus.arValidPipe() else bus
  def decoderToArbiterLink(bus: AxiLite4WriteOnly) = if(!lowLatency) bus.awValidPipe() else bus

  def addSlave(axi: AxiLite4Bus, mapping: SizeMapping): this.type = {
    axi match {
      case axi: AxiLite4 => {
        val readOnly = AxiLite4ReadOnly(axi.config).setCompositeName(axi, "readOnly", true)
        val writeOnly = AxiLite4WriteOnly(axi.config).setCompositeName(axi, "writeOnly", true)
        readOnly >> axi
        writeOnly >> axi
        axiLite4ToReadWriteOnly(axi) = readOnly :: writeOnly :: Nil
        addSlave(readOnly, mapping)
        addSlave(writeOnly, mapping)
      }
      case _ => {
        slavesConfigs(axi) = AxiLite4CrossbarSlaveConfig(mapping)
      }
    }
    this
  }

  def addSlaves(orders: (AxiLite4Bus, SizeMapping)*): this.type = {
    orders.foreach(order => addSlave(order._1, order._2))
    this
  }

  def addConnection(axi: AxiLite4Bus, slaves: Seq[AxiLite4Bus]): this.type = {
    slaves.foreach(slave => require(slave.config == axi.config,
      s"AxiLite4 crossbar : the master and one of its slaves do not share the same AxiLite4Config " +
      s"(${axi.config} versus ${slave.config}). The crossbar does not adapt the address width, the " +
      s"data width, nor the issuing capabilities."))
    val translatedSlaves = slaves.map(_ match {
      case that: AxiLite4 => axiLite4ToReadWriteOnly(that)
      case that: AxiLite4Bus => that :: Nil
    }).flatten
    axi match {
      case axi: AxiLite4 => {
        val readSlaves = translatedSlaves.filter(!_.isInstanceOf[AxiLite4WriteOnly])
        val writeSlaves = translatedSlaves.filter(!_.isInstanceOf[AxiLite4ReadOnly])
        require(readSlaves.nonEmpty && writeSlaves.nonEmpty,
          s"AxiLite4 crossbar : $axi is a read/write master, but its slaves are " +
          s"${if(readSlaves.isEmpty) "write" else "read"} only. Connect ${if(readSlaves.isEmpty) "toWriteOnly()" else "toReadOnly()"} " +
          s"instead, and tie off the other direction, for instance with an AxiLite4" +
          s"${if(readSlaves.isEmpty) "ReadOnly" else "WriteOnly"}ErrorSlave.")
        val readOnly = axi.toReadOnly().setCompositeName(axi, "readOnly", true)
        val writeOnly = axi.toWriteOnly().setCompositeName(axi, "writeOnly", true)
        axiLite4ToReadWriteOnly(axi) = readOnly :: writeOnly :: Nil
        addConnection(readOnly, readSlaves)
        addConnection(writeOnly, writeSlaves)
      }
      case axi: AxiLite4WriteOnly => {
        translatedSlaves.filter(!_.isInstanceOf[AxiLite4ReadOnly]).foreach(slavesConfigs(_).connections += AxiLite4CrossbarSlaveConnection(axi))
        masters += axi
      }
      case axi: AxiLite4ReadOnly => {
        translatedSlaves.filter(!_.isInstanceOf[AxiLite4WriteOnly]).foreach(slavesConfigs(_).connections += AxiLite4CrossbarSlaveConnection(axi))
        masters += axi
      }
    }
    this
  }

  def addConnection(order: (AxiLite4Bus, Seq[AxiLite4Bus])): this.type = addConnection(order._1, order._2)

  def addConnections(orders: (AxiLite4Bus, Seq[AxiLite4Bus])*): this.type = {
    orders.foreach(addConnection(_))
    this
  }

  def addPipelining(axi: AxiLite4ReadOnly)(bridger: (AxiLite4ReadOnly, AxiLite4ReadOnly) => Unit): this.type = {
    this.readOnlyBridger(axi) = bridger
    this
  }

  def addPipelining(axi: AxiLite4WriteOnly)(bridger: (AxiLite4WriteOnly, AxiLite4WriteOnly) => Unit): this.type = {
    this.writeOnlyBridger(axi) = bridger
    this
  }

  def addPipelining(axi: AxiLite4)(ro: (AxiLite4ReadOnly, AxiLite4ReadOnly) => Unit)(wo: (AxiLite4WriteOnly, AxiLite4WriteOnly) => Unit): this.type = {
    val b = axiLite4ToReadWriteOnly(axi)
    addPipelining(b(0).asInstanceOf[AxiLite4ReadOnly])(ro)
    addPipelining(b(1).asInstanceOf[AxiLite4WriteOnly])(wo)
    this
  }

  def build(): Unit = {
    val masterToDecodedSlave = mutable.HashMap[AxiLite4Bus, Map[AxiLite4Bus, AxiLite4Bus]]()

    def applyName(bus: Bundle, name: String, onThat: Nameable): Unit = {
      if(bus.component == Component.current)
        onThat.setCompositeName(bus, name)
      else if(bus.isNamed)
        onThat.setCompositeName(bus.component, bus.getName() + "_" + name)
    }

    val decoders = for(master <- masters) yield master match {
      case master: AxiLite4ReadOnly => new Area{
        val slaves = slavesConfigs.filter{
          case (slave, config) => config.connections.exists(connection => connection.master == master)
        }.toSeq

        val decoder = AxiLite4ReadOnlyDecoder(
          axiConfig = master.config,
          decodings = slaves.map(_._2.mapping),
          pendingMax = pendingMax
        )
        applyName(master, "decoder", decoder)
        masterToDecodedSlave(master) = (slaves.map(_._1), decoder.io.outputs.map(decoderToArbiterLink)).zipped.toMap
        readOnlyBridger.getOrElse[(AxiLite4ReadOnly, AxiLite4ReadOnly) => Unit](master, _ >> _).apply(master, decoder.io.input)
        readOnlyBridger.remove(master)
      }
      case master: AxiLite4WriteOnly => new Area{
        val slaves = slavesConfigs.filter{
          case (slave, config) => config.connections.exists(connection => connection.master == master)
        }.toSeq

        val decoder = AxiLite4WriteOnlyDecoder(
          axiConfig = master.config,
          decodings = slaves.map(_._2.mapping),
          pendingMax = pendingMax
        )
        applyName(master, "decoder", decoder)
        masterToDecodedSlave(master) = (slaves.map(_._1), decoder.io.outputs.map(decoderToArbiterLink)).zipped.toMap
        writeOnlyBridger.getOrElse[(AxiLite4WriteOnly, AxiLite4WriteOnly) => Unit](master, _ >> _).apply(master, decoder.io.input)
        writeOnlyBridger.remove(master)
      }
    }

    def plugRead(from: AxiLite4ReadOnly, to: AxiLite4ReadOnly): Unit = {
      readOnlyBridger.getOrElse[(AxiLite4ReadOnly, AxiLite4ReadOnly) => Unit](to, _ >> _).apply(from, to)
      readOnlyBridger.remove(to)
    }
    def plugWrite(from: AxiLite4WriteOnly, to: AxiLite4WriteOnly): Unit = {
      writeOnlyBridger.getOrElse[(AxiLite4WriteOnly, AxiLite4WriteOnly) => Unit](to, _ >> _).apply(from, to)
      writeOnlyBridger.remove(to)
    }

    val arbiters = for((slave, config) <- slavesConfigs.toSeq.sortBy(_._1.asInstanceOf[Bundle].getInstanceCounter)) yield slave match {
      case slave: AxiLite4ReadOnly => new Area{
        val readConnections = config.connections
        readConnections.size match {
          case 0 => PendingError(s"$slave has no master")
          case 1 => plugRead(masterToDecodedSlave(readConnections.head.master)(slave).asInstanceOf[AxiLite4ReadOnly], slave)
          case _ => new Area {
            val arbiter = AxiLite4ReadOnlyArbiter(
              outputConfig = slave.config,
              inputsCount = readConnections.length,
              routeBufferSize = routeBufferSize
            )
            applyName(slave, "arbiter", arbiter)
            for ((input, master) <- (arbiter.io.inputs, readConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[AxiLite4ReadOnly]
            }
            plugRead(arbiter.io.output, slave)
          }
        }
      }
      case slave: AxiLite4WriteOnly => new Area{
        val writeConnections = config.connections
        writeConnections.size match {
          case 0 => PendingError(s"$slave has no master")
          case 1 => plugWrite(masterToDecodedSlave(writeConnections.head.master)(slave).asInstanceOf[AxiLite4WriteOnly], slave)
          case _ => new Area {
            val arbiter = AxiLite4WriteOnlyArbiter(
              outputConfig = slave.config,
              inputsCount = writeConnections.length,
              routeBufferSize = routeBufferSize
            )
            applyName(slave, "arbiter", arbiter)
            for ((input, master) <- (arbiter.io.inputs, writeConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[AxiLite4WriteOnly]
            }
            plugWrite(arbiter.io.output, slave)
          }
        }
      }
    }
  }
}
