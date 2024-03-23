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

abstract class Pattern extends CCParameters {
  val width:    Int
  val prefix:   UInt
  val prefixOH: UInt

  /** used to detect whether a UInt match the pattern */
  def detect(in: UInt): Bool

  def compress(in: UInt): UInt
  def decompress(in: UInt): UInt

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

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    "b0".U
  }

  override def decompress(in: UInt): UInt = {
    0.U(ccEntryBits.W)
  }
}

object FourbitSignExt extends Pattern {
  override val width:    Int = 4
  override val prefix:   UInt = "b001".U
  override val prefixOH: UInt = "b00000010".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }

  override def decompress(in: UInt): UInt = {
    Cat(Fill(ccEntryBits - width, in(width - 1)), in)
  }
}

object OneByteSignExt extends Pattern {
  override val width:    Int = 8
  override val prefix:   UInt = "b010".U
  override val prefixOH: UInt = "b00000100".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }

  override def decompress(in: UInt): UInt = {
    Cat(Fill(ccEntryBits - width, in(width - 1)), in)
  }
}

object HalfWordSignExt extends Pattern {
  override val width:    Int = 16
  override val prefix:   UInt = "b011".U
  override val prefixOH: UInt = "b00001000".U

  override def detect(in: UInt) = detect_n_bit_signext(in)(width)

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }

  override def decompress(in: UInt): UInt = {
    Cat(Fill(ccEntryBits - width, in(width - 1)), in)
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

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(ccEntryBits - 1, ccEntryBits / 2)
  }

  override def decompress(in: UInt): UInt = {
    Cat(in, 0.U(width.W))
  }
}

object TwoSignExt extends Pattern {
  override val width:    Int = ccEntryBits / 2
  override val prefix:   UInt = "b101".U
  override val prefixOH: UInt = "b00100000".U
  private val halfEntryBits = ccEntryBits / 2
  private val quater = ccEntryBits / 4

  override def detect(in: UInt): Bool = {
    require(in.getWidth == ccEntryBits)
    val halfEntryBits = ccEntryBits / 2
    val quater = ccEntryBits / 4
    val halfEntry = (0 until 2).map(i => in((i + 1) * halfEntryBits - 1, i * halfEntryBits))
    halfEntry.map(x => x(halfEntryBits - 1, quater) === Fill(quater, x(quater - 1))).reduce(_ & _)
  }

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    Cat(in(halfEntryBits + quater - 1, halfEntryBits), in(quater - 1, 0))
  }

  override def decompress(in: UInt): UInt = {
    val num1 = Cat(Fill(quater, in(quater - 1)), in(quater - 1, 0))
    val num2 = Cat(Fill(quater, in(quater + halfEntryBits - 1)), in(quater + halfEntryBits - 1, 0))
    Cat(num2, num1)
  }
}

object RepeatedBytes extends Pattern {
  override val width:    Int = 8
  override val prefix:   UInt = "b110".U
  override val prefixOH: UInt = "b01000000".U

  private val repeat_num = ccEntryBits / width
  override def detect(in: UInt): Bool = {
    require(in.getWidth == ccEntryBits)
    val bytes = (0 until repeat_num).map(i => in(i * 8 + 7, i * 8))
    val first = bytes.head
    val remain = bytes.tail
    remain.map(_ === first).reduce(_ & _)
  }

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in(width - 1, 0)
  }

  override def decompress(in: UInt): UInt = {
    Fill(repeat_num, in)
  }
}

object UnCompressed extends Pattern {
  override val width:    Int = ccEntryBits
  override val prefix:   UInt = "b111".U
  override val prefixOH: UInt = "b10000000".U

  override def detect(in: UInt): Bool = true.B

  override def compress(in: UInt): UInt = {
    require(in.getWidth == ccEntryBits)
    in
  }

  override def decompress(in: UInt): UInt = in
}
