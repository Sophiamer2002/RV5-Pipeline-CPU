package test

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

class CPU extends RawModule {
    /******** begin parameter ***********/
    implicit val config = new Config {
        val DATA_WIDTH = 32
        val ADDR_WIDTH = 32
        val SRAM_DATA_WIDTH = 32
        val SRAM_ADDR_WIDTH = 20
        val CLK_FREQ = 10000000
        val BAUD = 115200
    }
    /******** end parameter ***********/

    /******* begin IO definition *******/
    val clk_50M = IO(Input(Bool()))
    val clk_11M0592 = IO(Input(Bool()))
    val push_btn = IO(Input(Bool()))
    val reset_btn = IO(Input(Bool()))
    val touch_btn = IO(Input(UInt(4.W)))
    val dip_sw = IO(Input(UInt(32.W)))
    val leds = IO(Output(UInt(16.W)))
    leds := DontCare
    val dpy0 = IO(Output(UInt(8.W)))
    dpy0 := DontCare
    val dpy1 = IO(Output(UInt(8.W)))
    dpy1 := DontCare
    val uart = IO(new Bundle() {
        val rdn = Output(Bool())
        val wrn = Output(Bool())
        val dataready = Input(Bool())
        val tbre = Input(Bool())
        val tsre = Input(Bool())
    })
    uart.rdn := true.B
    uart.wrn := true.B
    val base_ram = IO(new SRAM_Interface)
    val ext_ram = IO(new SRAM_Interface)
    val txd = IO(Output(Bool()))
    val rxd = IO(Input(Bool()))
    val flash = IO(new Bundle() {
        val a = Output(UInt(23.W))
        val d = Analog(16.W)
        val rp_n = Output(Bool())
        val vpen = Output(Bool())
        val ce_n = Output(Bool())
        val oe_n = Output(Bool())
        val we_n = Output(Bool())
        val byte_n = Output(Bool())
    })
    flash := DontCare
    val sl811 = IO(new Bundle() {
        val a0 = Output(Bool())
        val wr_n = Output(Bool())
        val rd_n = Output(Bool())
        val cs_n = Output(Bool())
        val rst_n = Output(Bool())
        val dack_n = Output(Bool())
        val intrq = Input(Bool())
        val drq_n = Input(Bool())
    })
    sl811 := DontCare
    val dm9k = IO(new Bundle() {
        val cmd = Output(Bool())
        val sd = Analog(16.W)
        val iow_n = Output(Bool())
        val ior_n = Output(Bool())
        val cs_n = Output(Bool())
        val pwrst_n = Output(Bool())
        val int = Input(Bool())
    })
    dm9k := DontCare
    val video = IO(new Bundle() {
        val red = Output(UInt(3.W))
        val green = Output(UInt(3.W))
        val blue = Output(UInt(2.W))
        val hsync = Output(Bool())
        val vsync = Output(Bool())
        val clk = Output(Bool())
        val de = Output(Bool())
    })
    video := DontCare
    /******** end IO definition ********/

    /******** begin clock generation ***/
    val clk_10M = Wire(Bool())
    val clk_20M = Wire(Bool())
    val locked = Wire(Bool())
    val clock_gen = Module(new pll_example)
    clock_gen.io.clk_in1 := clk_50M
    clk_10M := clock_gen.io.clk_out1
    clk_20M := clock_gen.io.clk_out2
    clock_gen.io.reset := reset_btn
    locked := !clock_gen.io.locked
    /******** end clock generation *****/

    val sys_clock = Wire(Clock())
    val sys_reset = Wire(Reset())
    withClockAndReset(sys_clock, sys_reset) {
        sys_clock := clk_10M.asClock()
        val reset_of_clk10M = RegNext(locked)
        sys_reset := reset_of_clk10M

        // pipelines
        val IF = Module(new FetchInstruction)
        val ID = Module(new DecodeInstruction)
        val EX = Module(new ExecuteInstruction)
        val MEM = Module(new MemoryInstruction)
        val WB = Module(new WriteBackInstruction)

        // peripherals
        val uart_c = Module(new uart_controller)
        val ext_sram_c = Module(new SRAM_Controller)
        val base_sram_c = Module(new SRAM_Controller)
        
        // wishbone
        val wb_mux = Module(
            new WishboneMutiplexer(config.DATA_WIDTH, config.ADDR_WIDTH, config.DATA_WIDTH/8, 2, 3)
        )

        // others
        val reg_file = Module(new RegFile)
        val stall_logic = Module(new StallLogic)
        val forward_logic = Module(new ForwardLogic)

        // connect peripherals
        base_sram_c.io.sram <> base_ram
        base_sram_c.io.wb <> wb_mux.io.slave(0)
        wb_mux.io.addr_config(0).addr := "h80000000".U
        wb_mux.io.addr_config(0).mask := "hFFC00000".U

        ext_sram_c.io.sram <> ext_ram
        ext_sram_c.io.wb <> wb_mux.io.slave(1)
        wb_mux.io.addr_config(1).addr := "h80400000".U
        wb_mux.io.addr_config(1).mask := "hFFC00000".U

        uart_c.io.clk_i <> sys_clock.asBool()
        uart_c.io.rst_i <> sys_reset.asBool()
        uart_c.io.wb_stb_i <> wb_mux.io.slave(2).stb
        uart_c.io.wb_cyc_i <> wb_mux.io.slave(2).cyc
        uart_c.io.wb_ack_o <> wb_mux.io.slave(2).ack
        uart_c.io.wb_adr_i <> wb_mux.io.slave(2).adr
        uart_c.io.wb_dat_i <> wb_mux.io.slave(2).data_in
        uart_c.io.wb_dat_o <> wb_mux.io.slave(2).data_out
        uart_c.io.wb_sel_i <> wb_mux.io.slave(2).sel
        uart_c.io.wb_we_i <> wb_mux.io.slave(2).we
        wb_mux.io.slave(2).err := false.B
        wb_mux.io.slave(2).rty := false.B

        uart_c.io.uart_rxd_i <> rxd
        uart_c.io.uart_txd_o <> txd

        wb_mux.io.addr_config(2).addr := "h10000000".U
        wb_mux.io.addr_config(2).mask := "hFFFF0000".U

        // connect pipelines
        IF.io.flow_out <> ID.io.flow_in
        ID.io.flow_out <> EX.io.flow_in
        EX.io.flow_out <> MEM.io.flow_in
        MEM.io.flow_out <> WB.io.flow_in

        // connect pipelines: register file
        ID.io.raddr_a <> reg_file.io.raddr_a
        ID.io.raddr_b <> reg_file.io.raddr_b
        ID.io.rdata_a <> reg_file.io.rdata_a
        ID.io.rdata_b <> reg_file.io.rdata_b

        WB.io.waddr <> reg_file.io.waddr
        WB.io.wdata <> reg_file.io.wdata
        WB.io.wen <> reg_file.io.wen

        WB.io.waddr <> ID.io.waddr
        WB.io.wdata <> ID.io.wdata
        WB.io.wen <> ID.io.wen

        // connect pipelines: wishbone
        IF.io.wbm <> wb_mux.io.master(1)
        MEM.io.wbm <> wb_mux.io.master(0)

        // connect pipelines: stall logic
        IF.io.stall_in <> stall_logic.io.IF_out
        IF.io.stall_out <> stall_logic.io.IF_in
        ID.io.stall_in <> stall_logic.io.ID_out
        ID.io.stall_out <> stall_logic.io.ID_in
        EX.io.stall_in <> stall_logic.io.EX_out
        EX.io.stall_out <> stall_logic.io.EX_in
        MEM.io.stall_out <> stall_logic.io.MEM_in

        IF.io.give_up <> EX.io.give_up
        IF.io.new_pc <> EX.io.new_pc

        // connect pipelines: forward logic
        forward_logic.io.ex_raddr_a <> EX.io.raddr_a
        forward_logic.io.ex_raddr_b <> EX.io.raddr_b
        forward_logic.io.ex_rdata_a <> EX.io.rdata_a
        forward_logic.io.ex_rdata_b <> EX.io.rdata_b

        forward_logic.io.forward_data_a <> EX.io.forward_data_a
        forward_logic.io.forward_data_b <> EX.io.forward_data_b
        forward_logic.io.stall <> EX.io.forward_stall

        forward_logic.io.mem_waddr <> MEM.io.waddr
        forward_logic.io.mem_wdata <> MEM.io.wdata
        forward_logic.io.mem_wen <> MEM.io.wen
        forward_logic.io.mem_data_done <> MEM.io.data_done

        forward_logic.io.wb_waddr <> WB.io.waddr
        forward_logic.io.wb_wdata <> WB.io.wdata
        forward_logic.io.wb_wen <> WB.io.wen
    }
}