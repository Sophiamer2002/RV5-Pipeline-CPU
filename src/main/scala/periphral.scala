package test

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

class AnalogIO(val DATA_WIDTH: Int) extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
        val data_in = Input(UInt(DATA_WIDTH.W))
        val data_out = Output(UInt(DATA_WIDTH.W))
        val data_io = Analog(DATA_WIDTH.W)
        val enable = Input(Bool())
    })

    setInline("AnalogIO.v",
        s"""
           |`default_nettype wire
           |module AnalogIO(
           |    input [${DATA_WIDTH-1}:0] data_in,
           |    output [${DATA_WIDTH-1}:0] data_out,
           |    inout [${DATA_WIDTH-1}:0] data_io,
           |    input enable
           |);
           |    assign data_out = data_io;
           |    assign data_io = enable ? data_in : 'bz;
           |endmodule
           |""".stripMargin)
}

class SRAM_Interface(implicit val config: Config) extends Bundle{
    val SRAM_ADDR_WIDTH = config.SRAM_ADDR_WIDTH
    val SRAM_DATA_WIDTH = config.SRAM_DATA_WIDTH
    val data = Analog(SRAM_DATA_WIDTH.W)
    val addr = Output(UInt(SRAM_ADDR_WIDTH.W))
    val be_n = Output(UInt((SRAM_DATA_WIDTH/8).W))
    val ce_n = Output(Bool())
    val oe_n = Output(Bool())
    val we_n = Output(Bool())
}

class SRAM_Controller(implicit val config: Config) extends Module {

    require(config.DATA_WIDTH % 8 == 0, "DATA_WIDTH must be a multiple of 8")
    require(config.SRAM_DATA_WIDTH % 8 == 0, "SRAM_DATA_WIDTH must be a multiple of 8")
    val io = IO(new Bundle {
        val sram = new SRAM_Interface
        val wb = new WishboneSlaveInterface(config.DATA_WIDTH, config.ADDR_WIDTH, config.DATA_WIDTH/8)
    })

    val idle :: write :: read :: Nil = Enum(3)
    val state = RegInit(idle)

    switch(state) {
        is(idle) { state := Mux(io.wb.cyc && io.wb.stb, Mux(io.wb.we, write, read), idle) }
        is(write) { state := idle }
        is(read) { state := idle }
    }

    io.sram.addr := io.wb.adr >> 2.U
    io.sram.be_n := ~io.wb.sel
    io.sram.ce_n := ~(io.wb.cyc && io.wb.stb)
    io.sram.oe_n := ~(io.wb.cyc && io.wb.stb && !io.wb.we)
    io.sram.we_n := ~(io.wb.cyc && io.wb.stb && io.wb.we && state === idle && ~clock.asBool())

    // 三态门控制sram的数据端口
    val buf = Module(new AnalogIO(config.SRAM_DATA_WIDTH))
    buf.io.data_in := io.wb.data_in
    buf.io.enable := io.wb.cyc && io.wb.stb && io.wb.we
    io.wb.data_out := buf.io.data_out
    io.sram.data <> buf.io.data_io

    io.wb.ack := state =/= idle
    io.wb.err := false.B
    io.wb.rty := false.B
}

class uart_controller(implicit val config: Config) extends BlackBox(Map(
    "ADDR_WIDTH" -> config.ADDR_WIDTH,
    "DATA_WIDTH" -> config.DATA_WIDTH,

    "CLK_FREQ" -> config.CLK_FREQ,
    "BAUD" -> config.BAUD
)) {
    val io = IO(new Bundle {
        val clk_i = Input(Bool())
        val rst_i = Input(Bool())

        // wishbone slave interface
        val wb_cyc_i = Input(Bool())
        val wb_stb_i = Input(Bool())
        val wb_ack_o = Output(Bool())
        val wb_adr_i = Input(UInt(config.ADDR_WIDTH.W))
        val wb_dat_i = Input(UInt(config.DATA_WIDTH.W))
        val wb_dat_o = Output(UInt(config.DATA_WIDTH.W))
        val wb_sel_i = Input(UInt((config.DATA_WIDTH/8).W))
        val wb_we_i = Input(Bool())

        // uart interface
        val uart_txd_o = Output(Bool())
        val uart_rxd_i = Input(Bool())
    })
}
