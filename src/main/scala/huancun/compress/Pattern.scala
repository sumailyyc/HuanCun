package huancun.compress

import chisel3._
import chisel3.util._
import math._
import huancun.HasHuanCunParameters
import org.chipsalliance.cde.config.Parameters

trait CCParameters {
  val ccRate = 2
  val ccEntryBits = 32
  val singleEntryPrefixBits = 3
  val ccPrefixBits = 48
}

trait Pattern extends CCParameters {
  val width:    Int
  val prefix:   UInt
  val prefixOH: UInt

  /** used to detect whether a UInt match the pattern */
  def detect(in: UInt): Bool

  def encode(in: UInt): UInt

  def detect_n_bit_signext(in: UInt)(signext_bit: Int) = {
    require(in.getWidth == ccEntryBits)
    in(ccEntryBits - 1, signext_bit) === Fill(ccEntryBits - signext_bit, in(signext_bit - 1))
  }
}

object ZeroRun extends Pattern {
  override val width = 0
  override val prefix = "b000".U
  override val prefixOH: UInt = "b00000001".U

  override def detect(in: UInt) = {
    require(in.getWidth == ccEntryBits)
    in === 0.U(ccEntryBits.W)
  }

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    "b0".U
  }
}

object FourbitSignExt extends Pattern {
  override val width:    Int = 4
  override val prefix:   UInt = "b001".U
  override val prefixOH: UInt = "b00000010".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }
}

object OneByteSignExt extends Pattern {
  override val width:    Int = 8
  override val prefix:   UInt = "b010".U
  override val prefixOH: UInt = "b00000100".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }
}

object HalfWordSignExt extends Pattern {
  override val width:    Int = 16
  override val prefix:   UInt = "b011".U
  override val prefixOH: UInt = "b00001000".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }
}

object PadHalfZero extends Pattern {
  override val width:    Int = ccEntryBits / 2
  override val prefix:   UInt = "b100".U
  override val prefixOH: UInt = "b00010000".U

  override def detect(in: UInt): Bool = {
    require(in.getWidth == ccEntryBits)
    val halfEntryBits = ccEntryBits / 2
    in(halfEntryBits - 1, 0) === 0.U(halfEntryBits.W)
  }

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(ccEntryBits - 1, ccEntryBits / 2)
  }
}

object TwoSignExt extends Pattern {
  override val width:    Int = ccEntryBits / 2
  override val prefix:   UInt = "b101".U
  override val prefixOH: UInt = "b00100000".U

  override def detect(in: UInt): Bool = {
    require(in.getWidth == ccEntryBits)
    val halfEntryBits = ccEntryBits / 2
    val quater = ccEntryBits / 4
    val halfEntry = (0 until 2).map(i => in((i + 1) * halfEntryBits - 1, i * halfEntryBits))
    halfEntry.map(x => x(halfEntryBits - 1, quater) === Fill(quater, x(quater - 1))).reduce(_ & _)
  }

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    val halfEntryBits = ccEntryBits / 2
    val quater = ccEntryBits / 4
    Cat(in(halfEntryBits + quater - 1, halfEntryBits), in(quater - 1, 0))
  }
}

object RepeatedBytes extends Pattern {
  override val width:    Int = 8
  override val prefix:   UInt = "b110".U
  override val prefixOH: UInt = "b01000000".U

  override def detect(in: UInt): Bool = {
    require(in.getWidth == ccEntryBits)
    val repeat_num = ccEntryBits / 8
    val bytes = (0 until repeat_num).map(i => in(i * 8 + 7, i * 8))
    val first = bytes.head
    val remain = bytes.tail
    remain.map(_ === first).reduce(_ & _)
  }

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }
}

object UnCompressed extends Pattern {
  override val width:    Int = ccEntryBits
  override val prefix:   UInt = "b111".U
  override val prefixOH: UInt = "b10000000".U

  override def detect(in: UInt): Bool = true.B

  override def encode(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in
  }
}
