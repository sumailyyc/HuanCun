package huancun.compress

import chisel3._
import chisel3.util._
import huancun.{HuanCunBundle, HuanCunModule}
import org.chipsalliance.cde.config.Parameters
import utility.RegNextN

class DecompressUnit(implicit p: Parameters) extends HuanCunModule with CCParameters {
  class DataIn extends HuanCunBundle {
    val data = UInt((beatBytes * 8).W)
    val needFirstBeat = Bool()
  }

  class DataOut extends HuanCunBundle {
    val beat = UInt((beatBytes * 8).W)
  }

  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new DataIn))
    val out = DecoupledIO(new DataOut)
  })

  val entry_num = ccPrefixBits / singleEntryPrefixBits
  val entry_per_beat = entry_num / 2
  val CC_offsetWidth = (log2Ceil(blockBytes * 8) + 1)

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

  val s1_valid = RegInit(false.B)
  val s1_ready = WireInit(true.B)
  val s2_valid = RegInit(false.B)
  val s2_ready = WireInit(true.B)
  val s3_valid = RegInit(false.B)
  val s3_ready = WireInit(true.B)
  val s4_valid = RegInit(false.B)
  val s4_ready = WireInit(true.B)

  // stage 1
  val prefix =
    Seq.tabulate(entry_num)(i => io.in.bits.data((i + 1) * singleEntryPrefixBits - 1, i * singleEntryPrefixBits))
  dontTouch(VecInit(prefix))
  val prefixOH = VecInit(prefix.map(UIntToOH(_, CC_offsetWidth)))
  val compressedData = io.in.bits.data(beatBytes * 8 - 1, ccPrefixBits)

  val entryWidth = VecInit(
    prefixOH.map(prefixOH =>
      Mux1H(
        patternSeq.zipWithIndex.map { case (pat, i) => prefixOH(i) -> pat.width.U }
      )
    )
  )
  val entryOffsetLow = Seq.fill(entry_per_beat)(Wire(UInt(CC_offsetWidth.W)))
  val (entryWidthLow, entryWidthHigh) = entryWidth.splitAt(entry_per_beat)
  val halfWidth = entryOffsetLow.zip(entryWidthLow).foldLeft(0.U(CC_offsetWidth.W)) {
    case (sum, (offset, width)) =>
      offset := sum
      sum +& width
  }

  io.in.ready := s1_ready
  s1_ready := s1_valid & s2_ready | ~s1_valid
  when (s1_ready) {
    s1_valid := io.in.valid
  }

  val s1_prefixOH = RegEnable(prefixOH, io.in.fire)
  val s1_entryWidthHigh = RegEnable(VecInit(entryWidthHigh), io.in.fire)
  val s1_entryOffsetLow = RegEnable(VecInit(entryOffsetLow), io.in.fire)
  val s1_compressedData = RegEnable(compressedData, io.in.fire)
  val s1_halfWidth = RegEnable(halfWidth, io.in.fire)
  val s1_needFirstBeat = RegEnable(io.in.bits.needFirstBeat, io.in.fire)

  // stage 2
  val entryOffsetHigh = Seq.fill(entry_per_beat)(Wire(UInt(CC_offsetWidth.W)))
  entryOffsetHigh.zip(s1_entryWidthHigh).foldLeft(s1_halfWidth) {
    case (sum, (offset, width)) =>
      offset := sum
      sum +& width
  }
  val (prefixOHLow, prefixOHHigh) = prefixOH.splitAt(entry_per_beat)

  s2_ready := s2_valid & s3_ready | ~s2_valid
  when (s2_ready) {
    s2_valid := s1_valid
  }
  val s2_fire = s2_ready & s1_valid

  val s2_prefixOH = RegEnable(Mux(s1_needFirstBeat, VecInit(prefixOHLow), VecInit(prefixOHHigh)), s2_fire)
  val s2_entryOffset = RegEnable(Mux(s1_needFirstBeat, s1_entryOffsetLow, VecInit(entryOffsetHigh)), s2_fire)
  val s2_compressedData = RegEnable(s1_compressedData, s2_fire)

  // stage 3
  val shifter = s2_entryOffset.map(offset => (s2_compressedData >> offset)(ccEntryBits - 1, 0))

  s3_ready := s3_valid & s4_ready | ~s3_valid
  when (s3_ready) {
    s3_valid := s2_valid
  }
  val s3_fire = s3_ready & s2_valid

  val s3_prefixOH = RegEnable(s2_prefixOH, s3_fire)
  val s3_entryData = RegEnable(VecInit(shifter), s3_fire)

  // stage 4
  val entryData = s3_entryData.zip(s3_prefixOH).map {
    case (data, prefixOH) =>
      Mux1H(
        patternSeq.zipWithIndex.map {
          case (pat, i) => (prefixOH(i) -> pat.decompress(data))
        }
      )
  }

  s4_ready := s4_valid & io.out.ready | ~s4_valid
  when (io.out.ready) {
    s4_valid := s3_valid
  }
  val s4_fire = s4_ready & s3_valid
  val s4_data = RegEnable(VecInit(entryData), s4_fire)

  io.out.bits.beat := Cat(s4_data)
  io.out.valid := s4_valid
}

object DecompressUnit {
  val latency = 5
}
