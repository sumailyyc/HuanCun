package huancun.compress

import chisel3._
import chisel3.util._
import huancun.{HuanCunBundle, HuanCunModule}
import org.chipsalliance.cde.config.Parameters

/** If dataIn isn't compressible, encode unit will not output the compressed value
  * Notice that you can't send two fristBeat
  * You must send lastBeat after firstBeat
  * 请务必传输两个完整的beat，第一个为first beat，第二个为last beat，通过isFirstBeat控制
  */
class CompressUnit(debug: Boolean = false)(implicit p: Parameters) extends HuanCunModule with CCParameters {
  // val io = IO(new HuanCunBundle() {
  //   val dataIn = Input(UInt((beatBytes * 8).W))
  //   val dataOut = Output(UInt((beatBytes * 8).W))
  //   // set to recognize whether the dataIn is the first beat, since XiangShan always transport 2 beats in 2 clk
  //   val firstBeat = Input(Bool())
  //   val lastBeat = Input(Bool())
  //   val finish = Output(Bool())
  //   val compressible = Output(Bool())
  // })
  class DataIn extends HuanCunBundle {
    val data = UInt((beatBytes * 8).W)
    val isFirstBeat = Bool()
  }

  class DataOut extends HuanCunBundle {
    val data = UInt((beatBytes * 8).W)
    val compressible = Bool()
  }

  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new DataIn))
    val out = DecoupledIO(new DataOut)
  })

  /** The order has been arranged according to the prefix */
  val patternSeq = Seq(
    ZeroRun,
    FourbitSignExt,
    OneByteSignExt,
    HalfWordSignExt,
    PadHalfZero,
    TwoSignExt,
    RepeatedBytes,
    UnCompressed
  ).sortBy(_.prefix.litValue)

  val entry_per_beat = beatBytes * 8 / ccEntryBits // current value is 8
  val offsetWidth = (log2Ceil(blockBytes * 8) + 1)

  class Stage1Bundle extends Bundle {
    val first = Bool()
    val last = Bool()
    val prefixOH = Vec(entry_per_beat, UInt(8.W))
    val data = Vec(entry_per_beat, UInt(ccEntryBits.W))
  }
  val s1 = RegInit(0.U.asTypeOf(new Stage1Bundle))
  val s1_valid = RegInit(false.B)
  val s1_ready = WireInit(true.B)

  class Stage2Bundle extends Bundle {
    val first = Bool()
    val last = Bool()
    val prefixOH = Vec(entry_per_beat, UInt(8.W))
    val data = Vec(entry_per_beat, UInt(ccEntryBits.W))
    val entryOffset = Vec(entry_per_beat, UInt(offsetWidth.W))
    val halfWid = UInt(offsetWidth.W)
    val compressible = Bool()
  }
  val s2 = RegInit(0.U.asTypeOf(new Stage2Bundle))
  val s2_valid = RegInit(false.B)
  val s2_ready = WireInit(true.B)

  class Stage3Bundle extends Bundle {
    val first = Bool()
    val last = Bool()
    val compressedData = UInt((beatBytes * 8).W)
    val prefix = UInt((entry_per_beat * 2 * 3).W)
    val compressible = Bool()
  }
  val s3 = RegInit(0.U.asTypeOf(new Stage3Bundle))
  val s3_valid = RegInit(false.B)
  val s3_ready = WireInit(true.B)

  dontTouch(s1_valid)
  dontTouch(s1_ready)
  dontTouch(s2_valid)
  dontTouch(s2_ready)
  dontTouch(s3_valid)
  dontTouch(s3_ready)

  // stage 1: frequent pattern detect
  val ccEntry = VecInit(
    (0 until entry_per_beat).map(i => io.in.bits.data((i + 1) * ccEntryBits - 1, i * ccEntryBits))
  )
  val patternMatcher =
    VecInit(
      ccEntry.map(entry =>
        MuxCase(patternSeq.last.prefixOH, patternSeq.init.map(pat => pat.detect(entry) -> pat.prefixOH))
      )
    )

  when(s1_ready) {
    s1_valid := io.in.valid
  }
  io.in.ready := s1_ready
  s1_ready := s1_valid & s2_ready | ~s1_valid

  when(io.in.fire) {
    s1.first := io.in.bits.isFirstBeat
    s1.last := ~io.in.bits.isFirstBeat
    s1.prefixOH := patternMatcher
    s1.data := ccEntry
  }

  // stage 2: zero run
  val entryWidth = s1.prefixOH.map(prefixOH =>
    Mux1H(
      patternSeq.zipWithIndex.map { case (pat, i) => prefixOH(i) -> pat.width.U }
    )
  )
  val entryOffset = Seq.fill(entry_per_beat)(Wire(UInt(offsetWidth.W)))
  val halfWidth = entryWidth.zip(entryOffset).foldLeft(0.U(offsetWidth.W)) {
    case (sum, (width, offset)) =>
      offset := sum
      sum +& width
  }

  // val s2_first = RegNext(s1_first)
  // val s2_last = RegNext(s1_last)
  // val s2_prefixOH = RegNext(s1_prefixOH)
  // val s2_data = RegNext(s1_data)
  // val s2_entryOffset = RegNext(VecInit(entryOffset))

  // val s2_halfWid = RegInit(0.U(offsetWidth))
  // val s2_compressible = RegInit(false.B)
  // when(s1_first) {
  //   s2_halfWid := halfWidth
  // }.elsewhen(s1_last) {
  //   s2_compressible := (s2_halfWid +& halfWidth) < (beatBytes * 8 - ccPrefixBits).U
  // }
  s2_ready := s2_valid & s3_ready | ~s2_valid
  when(s2_ready) {
    s2_valid := s1_valid
  }
  when(s1_valid && s2_ready) {
    s2.first := s1.first
    s2.last := s1.last
    s2.prefixOH := s1.prefixOH
    s2.data := s1.data
    s2.entryOffset := VecInit(entryOffset)
    when(s1.first) {
      s2.halfWid := halfWidth
    }.elsewhen(s1.last) {
      s2.compressible := (s2.halfWid +& halfWidth) < (beatBytes * 8 - ccPrefixBits).U
    }
  }

  if (debug) {
    val lengthLimit = WireInit((beatBytes * 8 - ccPrefixBits).U)
    val totalLength = s2.halfWid +& halfWidth +& WireInit(48.U)
    dontTouch(lengthLimit)
    dontTouch(totalLength)
  }

  // stage 3: gen data
  val encodedEntry = s2.prefixOH.zip(s2.data).map {
    case (prefix, data) =>
      Mux1H {
        patternSeq.zipWithIndex.map {
          case (pat, i) => prefix(i) -> pat.encode(data)
        }
      }
  }

  val halfCompressedData = encodedEntry
    .zip(s2.entryOffset)
    .map {
      case (entry, offset) =>
        val tmp = WireInit(0.U((beatBytes * 8 - ccPrefixBits).W))
        tmp := entry
        tmp << offset
    }
    .reduce(_ | _)

  // val s3_first = RegNext(s2_first)
  // val s3_last = RegNext(s2_last)

  // val s3_compressedData = RegInit(0.U((beatBytes * 8).W))
  // val s3_prefix = RegInit(0.U((entry_per_beat * 2 * 3).W))

  val s3_canReceiveFirst = Mux(s3_valid, s3.last & io.out.ready, true.B)
  val s3_canReceiveLast = s3_valid & s3.first
  s3_ready := s3_canReceiveFirst | s3_canReceiveLast

  when(s2_valid && s2.first && s3_canReceiveFirst) {
    s3.compressedData := halfCompressedData | 0.U((beatBytes * 8).W)
    s3.prefix := Cat(s2.prefixOH.reverse.map(prefixOH => OHToUInt(prefixOH)))
  }.elsewhen(s2_valid && s2.last && s3_canReceiveLast) {
    s3.compressedData := s3.compressedData | halfCompressedData << s2.halfWid
    s3.prefix := Cat(Cat(s2.prefixOH.reverse.map(prefixOH => OHToUInt(prefixOH))), s3.prefix(ccPrefixBits / 2 - 1, 0))
    s3.compressible := s2.compressible
  }
  when(s3_ready) {
    s3_valid := s2_valid
  }
  when(s2_valid && s3_ready) {
    s3.first := s2.first
    s3.last := s2.last
  }

  io.out.bits.data := Cat(s3.compressedData, s3.prefix)
  io.out.bits.compressible := s3.compressible
  io.out.valid := s3.last & s3_valid
}

object CompressUnit {
  val latency = 3

  def apply(in: UInt, first: Bool, last: Bool)(implicit p: Parameters) = {}
}
