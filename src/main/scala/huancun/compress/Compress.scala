package huancun.compress

import chisel3._
import chisel3.util._
import huancun.{HuanCunBundle, HuanCunModule}
import org.chipsalliance.cde.config.Parameters

/** if dataIn isn't compressible, encode unit will not output the compressed value */
class CompressUnit(debug: Boolean = false)(implicit p: Parameters) extends HuanCunModule with CCParameters {
  val io = IO(new HuanCunBundle() {
    val dataIn = Input(UInt((beatBytes * 8).W))
    val dataOut = Output(UInt((beatBytes * 8).W))
    // set to recognize whether the dataIn is the first beat, since XiangShan always transport 2 beats in 2 clk
    val begin = Input(Bool())
    val finish = Output(Bool())
    val compressible = Output(Bool())
  })

  // stage 1: frequent pattern detect

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

  val ccEntry = VecInit((0 until entry_per_beat).map(i => io.dataIn((i + 1) * ccEntryBits - 1, i * ccEntryBits)).toSeq)
  val patternMatcher =
    VecInit(
      ccEntry.map(entry =>
        MuxCase(UnCompressed.prefixOH, patternSeq.init.map(pat => pat.detect(entry) -> pat.prefixOH))
      )
    )

  val s1_first = RegNext(io.begin)
  val s1_last = RegNext(s1_first)
  val s1_prefixOH = RegNext(patternMatcher)
  val s1_data = RegNext(ccEntry)

  // stage 2: zero run
  val entryWidth = s1_prefixOH.map(prefixOH =>
    Mux1H(
      patternSeq.zipWithIndex.map { case (pat, i) => prefixOH(i) -> pat.width.U }
    )
  )
  val offsetWidth = (log2Ceil(blockBytes * 8) + 1).W
  val entryOffset = Seq.fill(entry_per_beat)(Wire(UInt(offsetWidth)))
  val halfWidth = entryWidth.zip(entryOffset).foldLeft(0.U((offsetWidth))) {
    case (sum, (width, offset)) =>
      offset := sum
      sum +& width
  }

  val s2_first = RegNext(s1_first)
  val s2_last = RegNext(s1_last)
  val s2_prefixOH = RegNext(s1_prefixOH)
  val s2_data = RegNext(s1_data)
  val s2_entryOffset = RegNext(VecInit(entryOffset))

  val s2_halfWid = RegInit(0.U(offsetWidth))
  val s2_compressible = RegInit(false.B)
  when(s1_first) {
    s2_halfWid := halfWidth
  }.elsewhen(s1_last) {
    s2_compressible := (s2_halfWid +& halfWidth) < (beatBytes * 8 - ccPrefixBits).U
  }

  if (debug) {
    val lengthLimit = WireInit((beatBytes * 8 - ccPrefixBits).U)
    val totalLength = s2_halfWid +& halfWidth +& WireInit(48.U)
    dontTouch(lengthLimit)
    dontTouch(totalLength)
  }

  // stage 3: gen data
  val encodedEntry = s2_prefixOH.zip(s2_data).map {
    case (prefix, data) =>
      Mux1H {
        patternSeq.zipWithIndex.map {
          case (pat, i) => prefix(i) -> pat.encode(data)
        }
      }
  }

  val halfCompressedData = encodedEntry
    .zip(s2_entryOffset)
    .map {
      case (entry, offset) =>
        val tmp = WireInit(0.U((beatBytes * 8 - ccPrefixBits).W))
        tmp := entry
        tmp << offset
    }
    .reduce(_ | _)(beatBytes * 8 - 1, 0)

  val s3_first = RegNext(s2_first)
  val s3_last = RegNext(s2_last)

  val s3_compressedData = RegInit(0.U((beatBytes * 8).W))
  val s3_prefix = RegInit(0.U((entry_per_beat * 2 * 3).W))
  when(s2_first) {
    s3_compressedData := halfCompressedData | 0.U((beatBytes * 8).W)
    s3_prefix := Cat(s2_prefixOH.reverse.map(prefixOH => OHToUInt(prefixOH)))
  }.elsewhen(s2_last) {
    s3_compressedData := s3_compressedData | halfCompressedData << s2_halfWid
    s3_prefix := Cat(Cat(s2_prefixOH.reverse.map(prefixOH => OHToUInt(prefixOH))), s3_prefix(ccPrefixBits / 2 - 1, 0))
  }

  io.dataOut := Cat(s3_compressedData, s3_prefix)
  io.finish := s3_last
  io.compressible := RegNext(s2_compressible)
}

object CompressUnit {
  val latency = 3

  def apply(in: UInt, begin: Bool)(implicit p: Parameters) = {
    val encodeUnit = Module(new CompressUnit)
    encodeUnit.io.dataIn := in
    encodeUnit.io.begin := begin
    (encodeUnit.io.dataOut, encodeUnit.io.finish, encodeUnit.io.compressible)
  }
}
