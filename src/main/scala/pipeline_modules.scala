package test

import chisel3._
import chisel3.util._

// module for hazard solution

class StallLogic extends Module {
    val io = IO(new Bundle {
        val IF_in = Input(new StallLogicInterface)
        val IF_out = Output(new StallLogicInterface)
        val ID_in = Input(new StallLogicInterface)
        val ID_out = Output(new StallLogicInterface)
        val EX_in = Input(new StallLogicInterface)
        val EX_out = Output(new StallLogicInterface)
        val MEM_in = Input(new StallLogicInterface)
    })

    io.EX_out.stall := io.MEM_in.stall
    io.ID_out.stall := io.EX_in.stall
    io.IF_out.stall := io.ID_in.stall

    io.EX_out.flush := io.MEM_in.flush
    io.ID_out.flush := io.EX_in.flush || io.MEM_in.flush
    io.IF_out.flush := io.ID_in.flush || io.EX_in.flush || io.MEM_in.flush
}

class ForwardLogic(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        // Execution stage
        val ex_raddr_a = Input(UInt(5.W))
        val ex_rdata_a = Input(UInt(config.DATA_WIDTH.W))
        val ex_raddr_b = Input(UInt(5.W))
        val ex_rdata_b = Input(UInt(config.DATA_WIDTH.W))

        // Memory stage
        val mem_waddr = Input(UInt(5.W))
        val mem_wdata = Input(UInt(config.DATA_WIDTH.W))
        val mem_wen = Input(Bool())
        val mem_data_done = Input(Bool())
        
        // Write back stage
        val wb_waddr = Input(UInt(5.W))
        val wb_wdata = Input(UInt(config.DATA_WIDTH.W))
        val wb_wen = Input(Bool())

        // output to execution stage
        val stall = Output(Bool()) // stall because of data hazard
        val forward_data_a = Output(UInt(config.DATA_WIDTH.W))
        val forward_data_b = Output(UInt(config.DATA_WIDTH.W))
    })

    // TODO: deal with data hazard
    io.forward_data_a := io.ex_rdata_a
    io.forward_data_b := io.ex_rdata_b
    io.stall := false.B

    when(io.ex_raddr_a === io.mem_waddr && io.mem_wen && io.ex_raddr_a =/= 0.U) {
        io.forward_data_a := io.mem_wdata
        io.stall := (~io.mem_data_done)
    }.elsewhen(io.ex_raddr_a === io.wb_waddr && io.wb_wen && io.ex_raddr_a =/= 0.U) {
        io.forward_data_a := io.wb_wdata
    }

    when(io.ex_raddr_b === io.mem_waddr && io.mem_wen && io.ex_raddr_b =/= 0.U) {
        io.forward_data_b := io.mem_wdata
        io.stall := (~io.mem_data_done)
    }.elsewhen(io.ex_raddr_b === io.wb_waddr && io.wb_wen && io.ex_raddr_b =/= 0.U) {
        io.forward_data_b := io.wb_wdata
    }
}

class RegFile(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        val raddr_a = Input(UInt(5.W))
        val rdata_a = Output(UInt(config.DATA_WIDTH.W))

        val raddr_b = Input(UInt(5.W))
        val rdata_b = Output(UInt(config.DATA_WIDTH.W))

        val waddr = Input(UInt(5.W))
        val wdata = Input(UInt(config.DATA_WIDTH.W))
        val wen = Input(Bool())
    })

    val regs = RegInit(VecInit(Seq.fill(32)(0.U(config.DATA_WIDTH.W))))

    io.rdata_a := regs(io.raddr_a)
    io.rdata_b := regs(io.raddr_b)

    when(io.waddr =/= 0.U && io.wen) {
        regs(io.waddr) := io.wdata
    }
}

class ImmGen(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        val instruction = Input(UInt(config.DATA_WIDTH.W))
        val imm = Output(UInt(32.W))
    })

    val opcode = io.instruction(6, 0)
    val imm = Wire(SInt(config.DATA_WIDTH.W))
    imm := 0.S
    when(opcode === "b0110111".U || opcode === "b0010111".U) {
        // lui, auipc
        imm := Cat(io.instruction(31, 12), 0.U(12.W)).asSInt
    }.elsewhen(opcode === "b1101111".U) {
        // jal
        imm := Cat(io.instruction(31), 
                      io.instruction(19, 12), 
                      io.instruction(20), 
                      io.instruction(30, 21), 
                      0.U(1.W)).asSInt
    }.elsewhen(opcode === "b1100111".U) {
        // jalr
        imm := io.instruction(31, 20).asSInt
    }.elsewhen(opcode === "b1100011".U) {
        // B type
        // BEQ, BNE, BLT, BGE, BLTU, BGEU
        imm := Cat(io.instruction(31), 
                      io.instruction(7), 
                      io.instruction(30, 25), 
                      io.instruction(11, 8), 
                      0.U(1.W)).asSInt
    }.elsewhen(opcode === "b0000011".U || opcode === "b0010011".U) {
        // I type
        // LB, LH, LW, LBU, LHU, ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI
        imm := io.instruction(31, 20).asSInt
    }.elsewhen(opcode === "b0100011".U) {
        // S type
        // SB, SH, SW
        imm := Cat(io.instruction(31, 25), io.instruction(11, 7)).asSInt
    }
    io.imm := imm.asUInt()
}

// module for pipeline stages

class FetchInstruction(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        val wbm = new WishboneMasterInterface(config.DATA_WIDTH, config.ADDR_WIDTH, 4)
        val flow_out = Output(new FetchDecodeIO)
        val stall_in = Input(new StallLogicInterface)
        val stall_out = Output(new StallLogicInterface)

        val new_pc = Input(UInt(config.ADDR_WIDTH.W))
        val give_up = Input(Bool())
    })

    val init :: waiting :: fake :: get :: Nil = Enum(4)

    val regs = new Bundle {
        val state = RegInit(init)
        val pc = RegInit(UInt(config.ADDR_WIDTH.W), "h80000000".U)
        val new_pc = RegInit("h80000004".U(config.ADDR_WIDTH.W))
        val instruction = RegInit(0.U(config.DATA_WIDTH.W))
    }

    val wires = new Bundle {
        val n_state = Wire(UInt(init.getWidth.W))
    }
    wires.n_state := init
    switch (regs.state) {
        is(init) {
            when(io.stall_in.flush) { wires.n_state := fake }
                .otherwise { wires.n_state := waiting }
        }
        is(waiting) {
            when(!io.wbm.ack) { wires.n_state := Mux(io.stall_in.flush, fake, waiting) }
                .elsewhen(io.stall_in.stall) { wires.n_state := get }
                .otherwise { wires.n_state := init }
        }
        is(fake) { wires.n_state := Mux(io.wbm.ack, init, fake) }
        is(get) { wires.n_state := Mux(io.stall_in.flush || (~io.stall_in.stall), init, get) }
    }

    // deal with all register values
    regs.state := wires.n_state

    when (io.wbm.ack) { regs.instruction := io.wbm.data_ret }
    
    when(wires.n_state === init) {
        regs.pc := Mux(io.give_up, io.new_pc, regs.new_pc)
        regs.new_pc := Mux(io.give_up, io.new_pc, regs.new_pc) + 4.U
    }.elsewhen(io.give_up) { regs.new_pc := io.new_pc }

    io.wbm.adr := regs.pc
    io.wbm.stb := regs.state =/= get
    io.wbm.cyc := regs.state =/= get
    io.wbm.we := false.B
    io.wbm.sel := "b1111".U
    io.wbm.data_send := 0.U

    io.flow_out.pc := regs.pc
    io.flow_out.instruction := Mux(regs.state === get, regs.instruction, io.wbm.data_ret)
    io.flow_out.flushed := ~(regs.state =/= fake && wires.n_state === init) || io.stall_in.flush || io.stall_out.stall

    io.stall_out.flush := false.B
    io.stall_out.stall := io.stall_in.stall || wires.n_state =/= init
}

class DecodeInstruction(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        // pipeline in out interface
        val flow_in = Input(new FetchDecodeIO)
        val flow_out = Output(new DecodeExecuteIO)

        // register file in out interface
        val raddr_a = Output(UInt(5.W))
        val rdata_a = Input(UInt(config.DATA_WIDTH.W))
        val raddr_b = Output(UInt(5.W))
        val rdata_b = Input(UInt(config.DATA_WIDTH.W))

        // deal with data hazard
        val wen = Input(Bool())
        val waddr = Input(UInt(5.W))
        val wdata = Input(UInt(config.DATA_WIDTH.W))
        
        // deal with control hazard
        val stall_in = Input(new StallLogicInterface)
        val stall_out = Output(new StallLogicInterface)
    })

    // IF/ID registers
    val regs = RegInit(new FetchDecodeIO, 0.U.asTypeOf(new FetchDecodeIO))
    when (~io.stall_out.stall) { regs := io.flow_in }

    // modules
    val imm_gen = Module(new ImmGen)
    imm_gen.io.instruction := regs.instruction

    val alu_opcode = Module(new ALU_Opcode)
    alu_opcode.io.opcode := regs.instruction(6, 0)
    alu_opcode.io.funct3 := regs.instruction(14, 12)
    alu_opcode.io.funct7 := regs.instruction(31, 25)

    val cmp_opcode = Module(new CMP_Opcode)
    cmp_opcode.io.opcode := regs.instruction(6, 0)
    cmp_opcode.io.funct3 := regs.instruction(14, 12)

    // IO: register file
    io.raddr_a := regs.instruction(19, 15)
    io.raddr_b := regs.instruction(24, 20)

    // IO: flow out
    io.flow_out.pc := regs.pc

    io.flow_out.opcode := regs.instruction(6, 0)
    io.flow_out.funct3 := regs.instruction(14, 12)

    io.flow_out.use_rs1 := 
        io.flow_out.opcode =/= "b0110111".U &&
        io.flow_out.opcode =/= "b0010111".U &&
        io.flow_out.opcode =/= "b1101111".U   // not U-type and J-type
    io.flow_out.rs1 := io.raddr_a
    io.flow_out.rs1_data := Mux((io.flow_out.rs1 === io.waddr) && io.wen, io.wdata, io.rdata_a)
    io.flow_out.use_rs2 :=
        io.flow_out.opcode === "b1100011".U ||
        io.flow_out.opcode === "b0110011".U ||
        io.flow_out.opcode === "b0100011".U    // B-type, R-type, S-type
    io.flow_out.rs2 := io.raddr_b
    io.flow_out.rs2_data := Mux((io.flow_out.rs2 === io.waddr) && io.wen, io.wdata, io.rdata_b)

    io.flow_out.imm := imm_gen.io.imm
    io.flow_out.op1 := alu_opcode.io.op1
    io.flow_out.op2 := alu_opcode.io.op2
    io.flow_out.alu_op := alu_opcode.io.alu_op

    io.flow_out.cmp_op := cmp_opcode.io.cmp_op

    io.flow_out.mem_wen := regs.instruction(6, 0) === "b0100011".U
    io.flow_out.mem_ren := regs.instruction(6, 0) === "b0000011".U

    io.flow_out.rf_w_from_mem := regs.instruction(6, 0) === "b0000011".U
    io.flow_out.rf_wen := regs.instruction(6, 0) =/= "b0100011".U && 
        regs.instruction(6, 0) =/= "b1100011".U &&     // not S-type and B-type
        io.flow_out.rd =/= 0.U                         // not x0, otherwise don't need to write
    io.flow_out.rd := regs.instruction(11, 7)

    io.flow_out.flushed := regs.flushed || io.stall_in.flush || io.stall_out.stall

    // IO: stall logic
    io.stall_out.stall := io.stall_in.stall && (~io.stall_in.flush) && (~regs.flushed)
    io.stall_out.flush := false.B
}

class ExecuteInstruction(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        // pipeline in out interface
        val flow_in = Input(new DecodeExecuteIO)
        val flow_out = Output(new ExcuteMemoryIO)

        // deal with data hazard
        val raddr_a = Output(UInt(5.W))
        val raddr_b = Output(UInt(5.W))
        val rdata_a = Output(UInt(config.DATA_WIDTH.W))
        val rdata_b = Output(UInt(config.DATA_WIDTH.W))

        val forward_data_a = Input(UInt(config.DATA_WIDTH.W))
        val forward_data_b = Input(UInt(config.DATA_WIDTH.W))
        val forward_stall = Input(Bool())

        // deal with control hazard
        val stall_in = Input(new StallLogicInterface)
        val stall_out = Output(new StallLogicInterface)

        val give_up = Output(Bool())
        val new_pc = Output(UInt(config.ADDR_WIDTH.W))
    })

    // ID/EX registers and wires
    val regs = RegInit(new DecodeExecuteIO, 0.U.asTypeOf(new DecodeExecuteIO))
    val has_flushed = RegInit(false.B) // doesn't need to flush IF twice
    val wires = Wire(new Bundle {
        val stall_because_of_data_hazard = Bool()
        val stall_because_of_previous_stage = Bool()
    })
    when (~io.stall_out.stall) { regs := io.flow_in }

    // modules
    val alu = Module(new ALU)
    alu.io.alu_op := regs.alu_op
    alu.io.a := Mux(regs.op1, regs.pc, io.forward_data_a)
    alu.io.b := Mux(regs.op2, regs.imm, io.forward_data_b)

    val cmp = Module(new CMP)
    cmp.io.cmp_op := regs.cmp_op
    cmp.io.a := io.forward_data_a
    cmp.io.b := io.forward_data_b

    // IO: flow out
    io.flow_out.pc := regs.pc

    io.flow_out.alu_result := Mux(
        regs.opcode === "b1101111".U || regs.opcode === "b1100111".U, // jal, jalr
        regs.pc + 4.U, alu.io.out
    )

    io.flow_out.mem_sel :=
        Mux(regs.funct3(1), "b1111".U,                    // word
            Mux(regs.funct3(0), 
                Mux(alu.io.out(1), "b1100".U, "b0011".U), // half word
                1.U << alu.io.out(1, 0)                   // byte
            )
        )
    io.flow_out.mem_unsigned := regs.funct3(2)
    io.flow_out.mem_wdata := 
        Mux(regs.funct3(1), regs.rs2_data,                       // word
            Mux(regs.funct3(0),
                regs.rs2_data(15, 0) << (alu.io.out(1) << 4),    // half word
                regs.rs2_data(7, 0) << (alu.io.out(1, 0) << 3)   // byte
            )
        )
    io.flow_out.mem_wen := regs.mem_wen
    io.flow_out.mem_ren := regs.mem_ren

    io.flow_out.rf_w_from_mem := regs.rf_w_from_mem
    io.flow_out.rf_wen := regs.rf_wen
    io.flow_out.rd := regs.rd

    io.flow_out.flushed := regs.flushed || io.stall_in.flush || io.stall_out.stall

    // IO: forward logic
    io.raddr_a := Mux(regs.use_rs1, regs.rs1, 0.U)
    io.rdata_a := Mux(regs.use_rs1, regs.rs1_data, 0.U)
    io.raddr_b := Mux(regs.use_rs2, regs.rs2, 0.U)
    io.rdata_b := Mux(regs.use_rs2, regs.rs2_data, 0.U)

    // IO: stall logic
    wires.stall_because_of_previous_stage := io.stall_in.stall
    wires.stall_because_of_data_hazard := io.forward_stall
    io.stall_out.stall := 
        (wires.stall_because_of_previous_stage || wires.stall_because_of_data_hazard) &&
        (~io.stall_in.flush) && (~regs.flushed)
    io.stall_out.flush := (~wires.stall_because_of_data_hazard) && (~regs.flushed) && cmp.io.out

    // IO: branch or jump
    io.give_up := (~has_flushed) && io.stall_out.flush && (~io.stall_in.flush) // TODO, if mem stage can flush, then we shall recover pc!!!
    io.new_pc := alu.io.out
    has_flushed := Mux(io.stall_out.stall, io.give_up || has_flushed, false.B)
}

class MemoryInstruction(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        // pipeline in out interface
        val flow_in = Input(new ExcuteMemoryIO)
        val flow_out = Output(new MemoryWriteBackIO)

        // Wishbone interface
        val wbm = new WishboneMasterInterface(config.DATA_WIDTH, config.ADDR_WIDTH, 4)

        // deal with data hazard
        val waddr = Output(UInt(5.W))
        val wdata = Output(UInt(config.DATA_WIDTH.W))
        val wen = Output(Bool())
        val data_done = Output(Bool())

        // deal with control hazard
        val stall_out = Output(new StallLogicInterface)
    })

    // EX/MEM registers
    val regs = RegInit(new ExcuteMemoryIO, 0.U.asTypeOf(new ExcuteMemoryIO))
    // val done = RegInit(false.B)
    val use_mem = (regs.mem_ren || regs.mem_wen) && (~regs.flushed)
    when (~io.stall_out.stall) { regs := io.flow_in }
    // when(~io.stall_out.stall) { done := false.B }
    //     .elsewhen(io.wbm.ack && (io.stall_out.stall)) { done := true.B }

    // IO: wishbone master interface
    io.wbm.adr := regs.alu_result
    io.wbm.stb := use_mem
    io.wbm.cyc := use_mem
    io.wbm.we := regs.mem_wen
    io.wbm.sel := regs.mem_sel
    io.wbm.data_send := regs.mem_wdata

    // IO: flow out
    io.flow_out.pc := regs.pc

    io.flow_out.alu_result := regs.alu_result

    val unsigned_mem_load_data = Wire(UInt(config.DATA_WIDTH.W))
    val signed_mem_load_data = Wire(SInt(config.DATA_WIDTH.W))
    unsigned_mem_load_data := 0.U
    signed_mem_load_data := 0.S
    when (regs.mem_ren) {
        switch (regs.mem_sel) {
            is("b1111".U) {
                unsigned_mem_load_data := io.wbm.data_ret
                signed_mem_load_data := io.wbm.data_ret.asSInt
            }
            is("b1100".U) {
                unsigned_mem_load_data := io.wbm.data_ret(31, 16)
                signed_mem_load_data := io.wbm.data_ret(31, 16).asSInt
            }
            is("b0011".U) {
                unsigned_mem_load_data := io.wbm.data_ret(15, 0)
                signed_mem_load_data := io.wbm.data_ret(15, 0).asSInt
            }
            is("b1000".U) {
                unsigned_mem_load_data := io.wbm.data_ret(31, 24)
                signed_mem_load_data := io.wbm.data_ret(31, 24).asSInt
            }
            is("b0100".U) {
                unsigned_mem_load_data := io.wbm.data_ret(23, 16)
                signed_mem_load_data := io.wbm.data_ret(23, 16).asSInt
            }
            is("b0010".U) {
                unsigned_mem_load_data := io.wbm.data_ret(15, 8)
                signed_mem_load_data := io.wbm.data_ret(15, 8).asSInt
            }
            is("b0001".U) {
                unsigned_mem_load_data := io.wbm.data_ret(7, 0)
                signed_mem_load_data := io.wbm.data_ret(7, 0).asSInt
            }
        }
    }
    io.flow_out.mem_load_data := Mux(regs.mem_unsigned, unsigned_mem_load_data, signed_mem_load_data.asUInt)

    io.flow_out.rf_w_from_mem := regs.rf_w_from_mem
    io.flow_out.rf_wen := regs.rf_wen
    io.flow_out.rd := regs.rd

    io.flow_out.flushed := regs.flushed || io.stall_out.stall

    // IO: forward logic
    io.waddr := regs.rd
    io.wdata := io.flow_out.mem_load_data
    io.wen := regs.rf_wen && (~regs.flushed)
    io.data_done := io.wbm.ack

    // IO: stall logic
    io.stall_out.flush := false.B
    io.stall_out.stall := use_mem && (~io.wbm.ack)
}

class WriteBackInstruction(implicit val config: Config) extends Module {
    val io = IO(new Bundle {
        // pipeline in out interface
        val flow_in = Input(new MemoryWriteBackIO)

        // deal with data hazard
        val waddr = Output(UInt(5.W))
        val wdata = Output(UInt(config.DATA_WIDTH.W))
        val wen = Output(Bool())
    })

    io.wen := io.flow_in.rf_wen && (~io.flow_in.flushed)
    io.waddr := io.flow_in.rd
    io.wdata := Mux(io.flow_in.rf_w_from_mem, io.flow_in.mem_load_data, io.flow_in.alu_result)
}