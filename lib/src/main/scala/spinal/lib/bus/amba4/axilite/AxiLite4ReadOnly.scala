package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

case class AxiLite4ReadOnly(config: AxiLite4Config) extends Bundle with IMasterSlave with AxiLite4Bus {
  val ar = Stream(AxiLite4Ax(config))
  val r  = Stream(AxiLite4R(config))

  def readCmd   = ar
  def readRsp   = r

  def >>(that: AxiLite4): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: AxiLite4): Unit = that >> this

  def >>(that: AxiLite4ReadOnly): Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that: AxiLite4ReadOnly): Unit = that >> this

  /** Insert a `validPipe` on the ar channel, to cut the combinatorial path of a decoder/arbiter pair. */
  def arValidPipe(): AxiLite4ReadOnly = {
    val sink = AxiLite4ReadOnly(config)
    sink.ar << this.ar.validPipe()
    sink.r  >> this.r
    sink
  }

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }
}
