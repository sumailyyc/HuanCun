package huancun.compress

import chisel3._
import chisel3.util._
import huancun.{HuanCunBundle, HuanCunModule}
import org.chipsalliance.cde.config.Parameters
import utility.RegNextN

class DecompressUnit(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new HuanCunBundle() {
    val dataIn = Input(UInt((beatBytes * 8).W))
    val beat = Input(UInt(beatBits.W))
    val dataOut = Output(UInt((beatBytes * 8).W))
    val compressed = Input(Bool())
  })
  io := DontCare
  io.dataOut := RegNextN(Mux(io.compressed, 0.U, io.dataIn), DecompressUnit.latency)
}

object DecompressUnit {
  val latency = 5
  def apply(in: UInt)(implicit p: Parameters) = {
    val decodeUnit = Module(new DecompressUnit())
    decodeUnit.io.dataIn := in
    decodeUnit.io.beat := 1.U
    decodeUnit.io.compressed := false.B
    decodeUnit.io.dataOut
  }
}
