package test

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

class WishboneSlaveInterface(val DATA_WIDTH: Int,
                             val ADDR_WIDTH: Int, 
                             val SELECT_WIDTH: Int) extends Bundle {
    val adr = Input(UInt(ADDR_WIDTH.W))
    val data_in = Input(UInt(DATA_WIDTH.W))
    val data_out = Output(UInt(DATA_WIDTH.W))
    val sel = Input(UInt(SELECT_WIDTH.W))
    val cyc = Input(Bool())
    val stb = Input(Bool())
    val we = Input(Bool())
    val ack = Output(Bool())
    val err = Output(Bool())
    val rty = Output(Bool())
}

class WishboneMasterInterface(val DATA_WIDTH: Int,
                              val ADDR_WIDTH: Int, 
                              val SELECT_WIDTH: Int) extends Bundle {
    val adr = Output(UInt(ADDR_WIDTH.W))
    val data_send = Output(UInt(DATA_WIDTH.W))
    val data_ret = Input(UInt(DATA_WIDTH.W))
    val sel = Output(UInt(SELECT_WIDTH.W))
    val stb = Output(Bool())
    val cyc = Output(Bool())
    val we = Output(Bool())
    val ack = Input(Bool())
    val err = Input(Bool())
    val rty = Input(Bool())
}

class WishboneSlaveAddrConfig(val ADDR_WIDTH: Int) extends Bundle {
    val addr = UInt(ADDR_WIDTH.W)
    val mask = UInt(ADDR_WIDTH.W)
}

class WishboneMutiplexer(val DATA_WIDTH: Int, val ADDR_WIDTH: Int, val SELECT_WIDTH: Int,
                         val NUM_MASTER: Int, val NUM_SLAVE: Int) extends Module {
    val io = IO(new Bundle {
        val slave = Vec(NUM_SLAVE, Flipped(new WishboneSlaveInterface(DATA_WIDTH, ADDR_WIDTH, SELECT_WIDTH)))
        val master = Vec(NUM_MASTER, Flipped(new WishboneMasterInterface(DATA_WIDTH, ADDR_WIDTH, SELECT_WIDTH)))
        val addr_config = Vec(NUM_SLAVE, Input(new WishboneSlaveAddrConfig(ADDR_WIDTH)))
    })

    // arbiter logic
    val reg = new Bundle{
        val chosen = RegInit(false.B)
        val master_sel = RegInit(0.U(log2Ceil(NUM_MASTER+1).W))
    }

    val request_vec = io.master.map(m => m.cyc && m.stb)
    val has_request = request_vec.reduce(_ || _)
    val ack = io.slave.map(_.ack).reduce(_ || _)

    val arbiter = PriorityMux(request_vec zip (0 until NUM_MASTER).map(_.U))
    reg.chosen := has_request && !ack
    reg.master_sel := Mux(!reg.chosen, arbiter, reg.master_sel)

    val cur_master_idx = Mux(reg.chosen, reg.master_sel, arbiter)

    // slave logic
    val slave_match = io.addr_config.map(c =>
        (((io.master(cur_master_idx).adr ^ c.addr) & c.mask).orR)
        ).map(~_)
    val select_error = (~slave_match.reduce(_ || _)) && has_request
    val cur_slave_idx = PriorityMux(slave_match zip (0 until NUM_SLAVE).map(_.U))
    (io.slave zipWithIndex).foreach {
        case (slave, idx) =>
            val sel = (cur_slave_idx === idx.U) && has_request && !select_error
            slave.cyc := io.master(cur_master_idx).cyc && io.master(cur_master_idx).stb && sel
            slave.stb := io.master(cur_master_idx).stb && sel
            slave.adr := io.master(cur_master_idx).adr
            slave.data_in := io.master(cur_master_idx).data_send
            slave.sel := io.master(cur_master_idx).sel
            slave.we := io.master(cur_master_idx).we && sel
    }

    // master logic
    val data_ret = io.slave(cur_slave_idx).data_out

    io.master zip (0 until NUM_MASTER).map(_.U) foreach {
        case (master, idx) =>
            val master_cycle = (idx === cur_master_idx) && has_request
            master.data_ret := data_ret
            master.ack := ack && master_cycle
            master.err := master_cycle && select_error && io.slave(cur_slave_idx).err
            master.rty := master_cycle && io.slave(cur_slave_idx).rty
    }
}