package test

abstract class Config {
    val DATA_WIDTH: Int
    val ADDR_WIDTH: Int
    val SRAM_DATA_WIDTH: Int
    val SRAM_ADDR_WIDTH: Int
    val CLK_FREQ: Int
    val BAUD: Int
}
