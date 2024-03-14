package huancun

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._

class RefillBufferRead(implicit p: Parameters) extends HuanCunBundle {
  val valid = Input(Bool())
  val beat = Input(UInt(beatBits.W))
  val id = Input(UInt(bufIdxBits.W))
  val ready = Output(Bool())
  val buffer_data = Output(new DSData)
  val last = Input(Bool())
}

class RefillBufferWrite(implicit p: Parameters) extends HuanCunBundle {
  val valid = Input(Bool())
  val beat = Input(UInt(beatBits.W))
  val data = Input(new DSData)
  val ready = Output(Bool())
  val id = Output(UInt(bufIdxBits.W))
}

/**
  *   RefillBuffer is used to reduce outer grant -> inner grant latency
  *   refill data can be bypassed to inner cache without go through SRAM
  */
class RefillBuffer(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val sourceDRead = new RefillBufferRead()
    val sinkDRead = new RefillBufferRead()
    val sinkDWrite = new RefillBufferWrite()
  })

  val buffer = Mem(bufBlocks, Vec(beatSize, new DSData()))
  val valids = RegInit(VecInit(Seq.fill(bufBlocks){
    VecInit(Seq.fill(beatSize){false.B})
  }))

  val (r1, r2, w) = (io.sourceDRead, io.sinkDRead, io.sinkDWrite)
  val rlast1 = r1.last
  val rlast2 = r2.last
  val wlast = w.beat.andR
  val wfirst = w.beat === 0.U

  r1.buffer_data := buffer(r1.id)(r1.beat)
  r1.ready := valids(r1.id)(r1.beat)
  r2.buffer_data := buffer(r2.id)(r2.beat)
  r2.ready := valids(r2.id)(r2.beat)

  when(r1.valid && r1.beat === 0.U){
    assert(r1.ready, "[%d] first beat must hit!", r1.id)
  }

  when(r2.valid && r2.beat === 0.U){
    assert(r2.ready, "[%d] first beat must hit!", r2.id)
  }

  when(r1.valid && r1.ready && rlast1){ // last beat
    // assert(valids(r.id).asUInt.andR, "[%d] attempt to invalidate a invalid entry", r.id)
    valids(r1.id).foreach(_ := false.B)
  }

  when(r2.valid && r2.ready && rlast2){
    valids(r2.id).foreach(_ := false.B)
  }

  val validMask = VecInit(valids.map(vec => vec.asUInt.orR)).asUInt
  val freeIdx = PriorityEncoder(~validMask)

  w.ready := Mux(wfirst, RegNext(!validMask.andR, true.B), true.B)
  w.id := Mux(wfirst,
    RegNext(freeIdx, 0.U),
    RegEnable(w.id, w.valid && w.ready && wfirst)
  )

  when(w.valid && w.ready){
    assert(!valids(w.id)(w.beat), "[%d] attempt to write a valid entry", w.id)
    valids(w.id)(w.beat) := true.B
    buffer(w.id)(w.beat) := w.data
  }

}
