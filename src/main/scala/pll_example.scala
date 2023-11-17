package test

import chisel3._

// interface for pll_example
class pll_example extends BlackBox {
  val io = IO(new Bundle{
    val clk_in1 = Input(Bool())
    val clk_out1 = Output(Bool())
    val clk_out2 = Output(Bool())
    val reset = Input(Bool())
    val locked = Output(Bool())
  })
}