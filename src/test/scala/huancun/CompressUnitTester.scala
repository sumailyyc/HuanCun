package huancun

import chisel3._
import org.chipsalliance.cde.config.Config
import chiseltest._
import chiseltest.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chiseltest.simulator.{VerilatorCFlags, VerilatorFlags}
import firrtl.AnnotationSeq
import firrtl.stage.RunFirrtlTransformAnnotation
import org.scalatest.flatspec._
import org.scalatest.matchers.should._
import huancun.compress.CompressUnit
import freechips.rocketchip.diplomacy.LazyModule
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorResponse.File
import scala.util.Random

abstract class Pattern {
  val prefix: String
  val width:  Int

  /** first: origin; second: compressed */
  def apply(): (String, String)

  val seed = 0
  val random = new Random()
  def nbit(n: Int) = (0 until n).map(_ => random.nextInt(2).toString).reduce(_ + _)
  def signext(data: String, wid: Int, total_width: Int = 32)(implicit pre: Seq[String]) = {
    val sign_bit = data(0)
    val mask = if (sign_bit == '0') pre(0) else pre(1)
    mask + data
  }
}

object ZeroRun extends Pattern {
  override val prefix: String = "000"
  override val width = 0

  val zero = Seq.fill(32)("0").reduce(_ + _)

  override def apply() = (zero, "")

  def detect(in: String) = in == zero
}

object FourbitSignExt extends Pattern {
  override val prefix = "001"
  override val width = 4

  implicit val pre = Seq(Seq.fill(28)("0").reduce(_ + _), Seq.fill(28)("1").reduce(_ + _))
  override def apply() = {
    var num_4bit = nbit(4)
    var origin = signext(num_4bit, 4)
    while (ZeroRun.detect(origin)) {
      num_4bit = nbit(4)
      origin = signext(num_4bit, 4)
    }
    (origin, num_4bit)
  }

  def detect(in: String) = (0 until 28).map(in(_) == in(28)).reduce(_ && _)
}

object OneByteSignExt extends Pattern {
  override val prefix = "010"
  override val width = 8

  implicit val pre = Seq(Seq.fill(24)("0").reduce(_ + _), Seq.fill(24)("1").reduce(_ + _))
  override def apply() = {
    var num_8bit = nbit(8)
    var origin = signext(num_8bit, 8)
    while (FourbitSignExt.detect(origin)) {
      num_8bit = nbit(8)
      origin = signext(num_8bit, 8)
    }
    (origin, num_8bit)
  }

  def detect(in: String) = (0 until 24).map(in(_) == in(24)).reduce(_ && _)
}

object HalfWordSignExt extends Pattern {
  override val prefix = "011"
  override val width = 16

  implicit val pre = Seq(Seq.fill(16)("0").reduce(_ + _), Seq.fill(16)("1").reduce(_ + _))
  override def apply() = {
    var num_16bit = nbit(16)
    var origin = signext(num_16bit, 8)
    while (OneByteSignExt.detect(origin)) {
      num_16bit = nbit(16)
      origin = signext(num_16bit, 16)
    }
    (origin, num_16bit)
  }

  def detect(in: String) = (0 until 16).map(in(_) == in(16)).reduce(_ && _)
}

object PadHalfZero extends Pattern {
  override val prefix = "100"
  override val width = 16

  val halfZero = Seq.fill(16)("0").reduce(_ + _)
  override def apply() = {
    var num = nbit(16)
    var origin = num + halfZero
    while (ZeroRun.detect(origin)) {
      num = nbit(16)
      origin = num + halfZero
    }
    (origin, num)
  }

  def detect(in: String) = {
    (16 until 32).map(in(_) == '0').reduce(_ && _)
  }
}

object TwoSignExt extends Pattern {
  override val prefix = "101"
  override val width = 16

  implicit val pre = Seq(Seq.fill(8)("0").reduce(_ + _), Seq.fill(8)("1").reduce(_ + _))
  override def apply() = {
    var num1 = nbit(8)
    var num2 = nbit(8)
    var snum1 = signext(num1, 8, 16)
    var snum2 = signext(num2, 8, 16)
    var origin = snum2 + snum1
    while (HalfWordSignExt.detect(origin) || PadHalfZero.detect(origin)) {
      num1 = nbit(8)
      num2 = nbit(8)
      snum1 = signext(num1, 8, 16)
      snum2 = signext(num2, 8, 16)
      origin = snum2 + snum1
    }
    (snum2 + snum1, num2 + num1)
  }

  def detect(in: String) = {
    (0 until 8).map(in(_) == in(8)).reduce(_ && _) && (16 until 24).map(in(_) == in(24)).reduce(_ && _)
  }
}

object RepeatedBytes extends Pattern {
  override val prefix = "110"
  override val width = 8

  override def apply() = {
    var num = nbit(8)
    var origin = Seq.fill(4)(num).reduce(_ + _)
    while (HalfWordSignExt.detect(origin) || PadHalfZero.detect(origin)) {
      num = nbit(8)
      origin = Seq.fill(4)(num).reduce(_ + _)
    }
    (origin, num)
  }

  def detect(in: String) = in.grouped(8).toList.distinct.length == 1
}

object UnCompressed extends Pattern {
  override val prefix = "111"
  override val width = 32

  override def apply() = {
    var num = nbit(32)
    while (
      HalfWordSignExt.detect(num) || PadHalfZero.detect(num) || TwoSignExt.detect(num) || RepeatedBytes.detect(num)
    ) {
      num = nbit(32)
    }
    (num, num)
  }

  def detect(in: String) = true
}

object Generater {
  val patSeq = Seq(
    ZeroRun,
    FourbitSignExt,
    OneByteSignExt,
    HalfWordSignExt,
    PadHalfZero,
    TwoSignExt,
    RepeatedBytes,
    UnCompressed
  )

  val random = new Random()

  // 1: origin data, 2: compressed data
  def genSubNum = {
    (0 until 8).map(_ => patSeq(random.nextInt(8)))
  }

  def genFullNum = {
    val a = genSubNum
    val b = genSubNum
    val a_data = a.map(_.apply())
    val b_data = b.map(_.apply())

    val data1 = a_data.map(_._1).reduce(_ + _)
    val data2 = b_data.map(_._1).reduce(_ + _)

    val data1_prefix = a.map(_.prefix).reduce(_ + _)
    val data2_prefix = b.map(_.prefix).reduce(_ + _)

    val compressed_data1 = a_data.map(_._2).reduce(_ + _)
    val compressed_data2 = b_data.map(_._2).reduce(_ + _)

    val data_prefix = data2_prefix + data1_prefix
    val compressed_data = compressed_data2 + compressed_data1

    val dataOut = compressed_data + data_prefix
    val totalWidth = a.map(_.width).reduce(_ + _) + b.map(_.width).reduce(_ + _) + 48
    println(s"totalwidth: ${totalWidth}")
    val compressible = totalWidth < 256
    (data1, data2, dataOut, compressible)
  }
}

class CompressUnitTester extends L2Tester with UseVerilatorBackend with DumpVCD {

  val system = LazyModule(new ExampleSystem())
  chisel3.stage.ChiselStage.elaborate(system.module)

  val compressUnit = chisel3.aop.Select
    .collectDeep[CompressUnit](system.module) {
      case compressUnit: CompressUnit =>
        compressUnit
    }
    .head

  def Poke(data: String, first: Boolean, valid: Boolean)(implicit dut: CompressUnit) = {
    dut.io.in.bits.data.poke(s"b$data".U)
    dut.io.in.bits.isFirstBeat.poke(first.B)
    dut.io.in.valid.poke(valid.B)
  }
  def PokeFirst(data: String)(implicit dut: CompressUnit) = Poke(data, true, true)
  def PokeLast(data: String)(implicit dut: CompressUnit) = Poke(data, false, true)
  def dataPoke(data:            String)(implicit dut:  CompressUnit) = dut.io.in.bits.data.poke(s"b$data".U)
  def firstBeatPoke(first:      Boolean)(implicit dut: CompressUnit) = dut.io.in.bits.isFirstBeat.poke(first.B)
  def inValidPoke(valid:        Boolean)(implicit dut: CompressUnit) = dut.io.in.valid.poke(valid.B)
  def inReadyPeek(implicit dut: CompressUnit) = dut.io.in.ready.peek()

  def dataPeek(implicit dut:         CompressUnit) = dut.io.out.bits.data.peek()
  def compressiblePeek(implicit dut: CompressUnit) = dut.io.out.bits.compressible.peek()
  def outValidPeek(implicit dut:     CompressUnit) = dut.io.out.valid.peek()
  def outReadyPoke(ready:            Boolean)(implicit dut: CompressUnit) = dut.io.out.ready.poke(ready.B)

  def Expect(data: String, valid: Boolean, compressible: Boolean)(implicit dut: CompressUnit) = {
    dut.io.out.bits.data.expect(s"b$data".U)
    dut.io.out.valid.expect(valid.B)
    dut.io.out.bits.compressible.expect(compressible.B)
  }
  def dataExpect(data:      String)(implicit dut:  CompressUnit) = dut.io.out.bits.data.expect(s"b$data".U)
  def compressibleExpect(b: Boolean)(implicit dut: CompressUnit) = dut.io.out.bits.compressible.expect(b.B)
  def outValidExpect(b:     Boolean)(implicit dut: CompressUnit) = dut.io.out.valid.expect(b.B)
  def inReadyExpect(b:      Boolean)(implicit dut: CompressUnit) = dut.io.in.ready.expect(b.B)

  it should "do something" in {
    test(new CompressUnit(true)(compressUnit.p)).withAnnotations(testAnnos) { d =>
      implicit val dut = d
      dut.clock.setTimeout(200000)
      println("hello")
      // init
      dut.clock.step(1)
      dut.io.in.valid.poke(false.B)
      dut.clock.step(1)
      // val dataSeq = Seq("12121212", "12345678", "fff7fff8", "12340000", "00007fff", "0000007f", "00000007", "00000000")
      // val data1 = BigInt(dataSeq.reduce(_ + _), 16)
      // val data2 = BigInt(dataSeq.reverse.reduce(_ + _), 16)

      // test correction of compress
      dut.io.out.ready.poke(true.B)
      for (i <- 0 until 3) {
        dut.io.in.valid.poke(false.B)
        dut.clock.step(1)
        println(i)
        val (data1, data2, dataOut, compressible) = Generater.genFullNum
        PokeFirst(data1)
        dut.clock.step(1)
        PokeLast(data2)
        dut.clock.step(1)
        dut.io.in.valid.poke(false.B)
        while (dut.io.out.valid.peek.litToBoolean != true) {
          dut.clock.step(1)
        }
        println("data1: " + s"b${data1}".U.litValue.toString(16))
        println("data2: " + s"b${data2}".U.litValue.toString(16))
        println("dut: " + dut.io.out.bits.data.peek().litValue.toString(16))
        println("ref: " + s"b${dataOut}".U.litValue.toString(16))
        println()
        dut.io.out.bits.compressible.expect(compressible.B)
        if (compressible) {
          dut.io.out.bits.data.expect(s"b${dataOut}".U)
        }
        dut.clock.step(1)
      }

      // test the pipeline
      // when agent isn't ready
      var dat: String = "0"

      {
        val (data1, data2, dataOut, compressible) = Generater.genFullNum
        PokeFirst(data1)
        outReadyPoke(false)
        dut.clock.step()
        PokeLast(data2)
        while (dut.io.out.valid.peek.litToBoolean != true) {
          dut.clock.step(1)
          inValidPoke(false)
        }
        if (compressible) {
          dataExpect(dataOut)
        }
        dat = dataOut
        inReadyExpect(true)

        val (data3, data4, dataOut3, compressible3) = Generater.genFullNum
        PokeFirst(data3)
        dut.clock.step()
        PokeLast(data4)
        while (dut.io.out.valid.peek.litToBoolean != true) {
          dut.clock.step(1)
          inValidPoke(false)
        }
        if (compressible) {
          dataExpect(dataOut)
        }
        inReadyExpect(true)

        val (data5, data6, dataOut5, compressible5) = Generater.genFullNum
        PokeFirst(data5)
        dut.clock.step()
        PokeLast(data6)
        dut.clock.step(1)
        inValidPoke(false)
        dut.clock.step(2)
        if (compressible) {
          dataExpect(dataOut)
        }
        inReadyExpect(false)

        outReadyPoke(true)
        dut.clock.step()
        outValidExpect(false)
      }

    }
  }

}
