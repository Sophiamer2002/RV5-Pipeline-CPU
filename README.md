# RV5-Pipeline-CPU

### How to use?

To build the project, you can use either IntelliJ IDEA or VSCode with metals enabled, or even with sbt command line tools. Run and you will find a directory called `out-files\` containing files including `AnalogIO.v`, `CPU.v`. These two are used to build the FPGA. REMEMBER TO PREPEND A LINE
```verilog
`default_nettype wire
```
TO `CPU.v`.

In `others\` folder, `cpu.wcfg` is used for viewing the waves in vivado2019.2. To implement build the CPU, https://github.com/thu-cs-lab/thinpad_top provides enough files so that you can generate the bitstream.
