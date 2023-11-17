package test

import chisel3._

// some signal bundles

class StallLogicInterface extends Bundle {
    val stall = Bool()
    val flush = Bool()
}

// interface for pipeline stages

class FetchDecodeIO(implicit val config: Config) extends Bundle {
    val pc = UInt(config.ADDR_WIDTH.W)
    val instruction = UInt(config.DATA_WIDTH.W)
    val flushed = Bool()
}

class DecodeExecuteIO(implicit val config: Config) extends Bundle {
    val pc = UInt(config.ADDR_WIDTH.W)

    val opcode = UInt(7.W)
    val funct3 = UInt(3.W)

    // Excution signals
    val use_rs1 = Bool()
    val rs1 = UInt(5.W)
    val rs1_data = UInt(config.DATA_WIDTH.W)
    val use_rs2 = Bool()
    val rs2 = UInt(5.W)
    val rs2_data = UInt(config.DATA_WIDTH.W)

    val imm = UInt(32.W)
    val op1 = Bool() // 0: rs1, 1: pc
    val op2 = Bool() // 0: rs2, 1: imm
    val alu_op = ALU.op_type.cloneType
    
    val cmp_op = CMP.op_type.cloneType

    // Memory signals
    val mem_wen = Bool()
    val mem_ren = Bool()

    // Write back signals
    val rf_w_from_mem = Bool()
    val rf_wen = Bool()
    val rd = UInt(5.W)

    val flushed = Bool()
}

class ExcuteMemoryIO(implicit val config: Config) extends Bundle {
    val pc = UInt(config.ADDR_WIDTH.W)

    val alu_result = UInt(config.DATA_WIDTH.W)

    // Memory signals
    val mem_sel = UInt(4.W)
    val mem_unsigned = Bool()
    val mem_wdata = UInt(config.DATA_WIDTH.W)
    val mem_wen = Bool()
    val mem_ren = Bool()

    // Write back signals
    val rf_w_from_mem = Bool()
    val rf_wen = Bool()
    val rd = UInt(5.W)

    val flushed = Bool()
}

class MemoryWriteBackIO(implicit val config: Config) extends Bundle {
    val pc = UInt(config.ADDR_WIDTH.W)

    val alu_result = UInt(config.DATA_WIDTH.W)
    val mem_load_data = UInt(config.DATA_WIDTH.W)

    // Write back signals
    val rf_w_from_mem = Bool()
    val rf_wen = Bool()
    val rd = UInt(5.W)

    val flushed = Bool()
}