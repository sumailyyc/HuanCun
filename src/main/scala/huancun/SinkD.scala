/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import TLMessages.{AccessAckData, ReleaseAck}

class SinkD(edge: TLEdgeOut)(implicit p: Parameters) extends HuanCunModule {
  val io = IO(new Bundle() {
    val d = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val bs_waddr = DecoupledIO(new DSAddress)
    val bs_wdata = Output(new DSData)
    val bypass_read = Flipped(new RefillBufferRead)
    val bypass_write = Flipped(new RefillBufferWrite)
    val task = Flipped(DecoupledIO(new SinkDReq))
    val taskack = ValidIO(new SinkDAck)
    val way = Input(UInt(wayBits.W))
    val set = Input(UInt(setBits.W))
    val tag = Input(UInt(tagBits.W))
    val inner_grant = Input(Bool())  // sourceD will use bypass data
    val save_data_in_bs = Input(Bool())
    val resp = ValidIO(new SinkDResp)
    val sourceD_r_hazard = Flipped(ValidIO(new SourceDHazard))
  })

  val beats = blockBytes / beatBytes
  val (first, last, _, beat) = edge.count(io.d)
  val cache = io.save_data_in_bs
  val needData = io.d.bits.opcode(0)
  assert(!io.d.valid || !needData || io.d.bits.size === log2Up(blockBytes).U, "SinkD must receive aligned message when needData")

  val bypass_ready = (io.inner_grant || cache)  && needData && io.bypass_write.ready

  val task = io.task.bits
  val task_r = RegEnable(io.task.bits, io.task.fire)
  when(io.task.fire) {
    assert(io.task.bits.save, "SinkD must always accept a save task")
  }
  val busy = RegInit(false.B)
  when(io.task.fire) {
    busy := true.B
  }
  val task_w_safe = !(io.sourceD_r_hazard.valid &&
    io.sourceD_r_hazard.bits.safe(task.set, task.way))
  val w_counter_save = RegInit(0.U(beatBits.W))
  val w_fire_save = io.bs_waddr.fire && !io.bs_waddr.bits.noop
  when(w_fire_save) {
    w_counter_save := w_counter_save + 1.U
  }
  val w_save_done = w_counter_save === (beats - 1).U && w_fire_save
  val w_save_done_r = RegNext(w_save_done)
  when(w_save_done) {
    w_counter_save := 0.U
    busy := false.B
  }

  io.d.ready := !needData || (!cache && !io.inner_grant) || bypass_ready 

  // Generate resp
  io.resp.valid := (first || last) && io.d.fire // MSHR need to be notified when both first & last
  io.resp.bits.last := last
  io.resp.bits.opcode := io.d.bits.opcode
  io.resp.bits.param := io.d.bits.param
  io.resp.bits.source := io.d.bits.source
  io.resp.bits.sink := io.d.bits.sink
  io.resp.bits.denied := io.d.bits.denied
  io.resp.bits.dirty := io.d.bits.echo.lift(DirtyKey).getOrElse(false.B)
  io.resp.bits.bufIdx := io.bypass_write.id

  io.task.ready := !busy && task_w_safe
  io.taskack.bits.sink := RegNext(task_r.source)
  io.taskack.valid := RegNext(busy && w_save_done)

  // Save data to Datastorage
  io.bs_waddr.valid := busy && task_r.save && io.bypass_read.valid && io.bypass_read.ready && !w_save_done_r
  io.bs_waddr.bits.way := task_r.way
  io.bs_waddr.bits.set := task_r.set
  io.bs_waddr.bits.tag := task_r.tag
  io.bs_waddr.bits.beat := w_counter_save
  io.bs_waddr.bits.write := true.B
  io.bs_waddr.bits.noop := !(busy && task_r.save)
  io.bs_wdata := io.bypass_read.buffer_data

  io.bypass_write.valid := io.d.valid && bypass_ready
  io.bypass_write.beat := beat
  io.bypass_write.data.data := io.d.bits.data
  io.bypass_write.data.corrupt := false.B

  io.bypass_read.valid := busy && task_r.save && io.bs_waddr.ready
  io.bypass_read.beat := w_counter_save
  io.bypass_read.id := task_r.bufIdx
  io.bypass_read.last := task_r.cleanBuf && w_counter_save === (beats - 1).U
}
