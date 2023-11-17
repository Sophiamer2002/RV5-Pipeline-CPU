package test

import chisel3._
import chisel3.util._

object ALU {
    val add :: sub :: sll :: slt :: sltu :: xor :: srl :: sra :: or :: and :: imm :: Nil = Enum(11)
    val op_type = UInt(4.W)
}

class ALU(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        val alu_op = Input(ALU.op_type)
        val a = Input(UInt(config.DATA_WIDTH.W))
        val b = Input(UInt(config.DATA_WIDTH.W))
        val out = Output(UInt(config.DATA_WIDTH.W))
    })
    
    io.out := 0.U
    switch(io.alu_op) {
        is (ALU.add) { io.out := io.a + io.b }
        is (ALU.sub) { io.out := io.a - io.b }
        is (ALU.sll) { io.out := io.a << io.b(4, 0) }
        is (ALU.slt) { io.out := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U) }
        is (ALU.sltu) { io.out := Mux(io.a < io.b, 1.U, 0.U) }
        is (ALU.xor) { io.out := io.a ^ io.b }
        is (ALU.srl) { io.out := io.a >> io.b(4, 0) }
        is (ALU.sra) { io.out := (io.a.asSInt >> io.b(4, 0)).asUInt }
        is (ALU.or) { io.out := io.a | io.b }
        is (ALU.and) { io.out := io.a & io.b }
        is (ALU.imm) { io.out := io.b }
    }
}

class ALU_Opcode extends Module {
    val io = IO(new Bundle {
        val opcode = Input(UInt(7.W))
        val funct3 = Input(UInt(3.W))
        val funct7 = Input(UInt(7.W))
        val alu_op = Output(ALU.op_type)
        val op1 = Output(Bool()) // 0: rs1, 1: pc
        val op2 = Output(Bool()) // 0: rs2, 1: imm
    })

    io.alu_op := ALU.add
    when(io.opcode === "b0110011".U || io.opcode === "b0010011".U) {
        // R-type, I-type
        switch (io.funct3) {
            is ("b000".U) { io.alu_op := Mux(((io.opcode === "b0110011".U) && io.funct7(5)), ALU.sub, ALU.add) }
            is ("b001".U) { io.alu_op := ALU.sll }
            is ("b010".U) { io.alu_op := ALU.slt }
            is ("b011".U) { io.alu_op := ALU.sltu }
            is ("b100".U) { io.alu_op := ALU.xor }
            is ("b101".U) { io.alu_op := Mux(io.funct7(5), ALU.sra, ALU.srl) }
            is ("b110".U) { io.alu_op := ALU.or }
            is ("b111".U) { io.alu_op := ALU.and }
        }
    }.elsewhen(io.opcode === "b0110111".U) {
        // LUI
        io.alu_op := ALU.imm
    }

    // B-type, J-type, U-type
    io.op1 := true.B
    io.op2 := true.B

    switch(io.opcode) {
        is("b0110011".U) {
            // R-type
            io.op1 := false.B
            io.op2 := false.B
        }
        is("b0010011".U) {
            // I-type, imm
            io.op1 := false.B
            io.op2 := true.B
        }
        is("b0000011".U) {
            // I-type, load
            io.op1 := false.B
            io.op2 := true.B
        }
        is("b1100111".U) {
            // I-type, jalr
            io.op1 := false.B
            io.op2 := true.B
        }
        is("b0100011".U) {
            // S-type
            io.op1 := false.B
            io.op2 := true.B
        }
    }
}

object CMP {
    val unused :: eq :: ne :: lt :: ge :: ltu :: geu :: jump :: Nil = Enum(8)
    val op_type = UInt(3.W)
}

class CMP(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        val cmp_op = Input(CMP.op_type)
        val a = Input(UInt(config.DATA_WIDTH.W))
        val b = Input(UInt(config.DATA_WIDTH.W))
        val out = Output(Bool())
    })

    io.out := false.B
    switch (io.cmp_op) {
        is (CMP.eq) { io.out := io.a === io.b }
        is (CMP.ne) { io.out := io.a =/= io.b }
        is (CMP.lt) { io.out := io.a.asSInt < io.b.asSInt }
        is (CMP.ge) { io.out := io.a.asSInt >= io.b.asSInt }
        is (CMP.ltu) { io.out := io.a < io.b }
        is (CMP.geu) { io.out := io.a >= io.b }
        is (CMP.jump) { io.out := true.B }
    }
}

class CMP_Opcode extends Module {
    val io = IO(new Bundle {
        val opcode = Input(UInt(7.W))
        val funct3 = Input(UInt(3.W))
        val cmp_op = Output(CMP.op_type)
    })

    io.cmp_op := CMP.unused
    when(io.opcode === "b1100011".U) {
        // B-type
        switch(io.funct3) {
            is ("b000".U) { io.cmp_op := CMP.eq }
            is ("b001".U) { io.cmp_op := CMP.ne }
            is ("b100".U) { io.cmp_op := CMP.lt }
            is ("b101".U) { io.cmp_op := CMP.ge }
            is ("b110".U) { io.cmp_op := CMP.ltu }
            is ("b111".U) { io.cmp_op := CMP.geu }
        }
    }.elsewhen(io.opcode === "b1101111".U || io.opcode === "b1100111".U) {
        // jal, jalr
        io.cmp_op := CMP.jump
    }
}