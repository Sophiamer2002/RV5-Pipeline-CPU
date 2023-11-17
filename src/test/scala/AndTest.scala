// ANDtest.scala
package test

import chisel3._
import chisel3.util._
import chisel3.tester._

class testModule extends Module {
    val io = IO(new Bundle{
        val io1 = Input(UInt(32.W))
        val io2 = Output(UInt(32.W))
    })

    io.io2 := io.io1
}

object testMain extends App {
    Driver.execute(args ++ Array("-td", ".\\out-files\\"), () => new CPU)

    // RawTester.test(new ImmGen) { c =>
    //     c.io.instruction.poke("b1_1011010100_0_01001010_10101_1101111".U)
    //     print(s"imm: ${c.io.imm.peek()}\n")
    // }
}