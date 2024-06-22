package nebula.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import nebula._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import nebula.cpu.mem._
import nebula.cpu._
import nebula.cpu.fetch._
import spinal.lib.bus.amba3.ahblite._
import nebula.util._

case class MemStreamBench(cfg: NebulaCfg, width: Int) extends Component {
    val io = new Bundle {
        val jump = in     port Bool()
        val addr = in     port UInt(cfg.vaddrWidth bits)
        val dout = master port Stream(MemStreamPacket(cfg, width))
    }
    val reader = MemStreamReader(cfg, width, 0, true)
    val mem    = new AlignedAhb3Rom(
        AhbLite3Config(cfg.vaddrWidth, cfg.memWidth),
        "Hello, World".getBytes()
        // {
        //     val raw = "Hello, World!"
        //     val rom = Array.ofDim[BigInt]((raw.length * 8 - 1) / cfg.memWidth + 1)
        //     for (i <- 0 until rom.length) rom(i) = 0
        //     for (i <- 0 until raw.length) {
        //         rom(i * 8 / cfg.memWidth) |= raw(i) << (i * 8 % cfg.memWidth)
        //     }
        //     for (x <- rom) yield B(x, cfg.memWidth bits)
        // }
    )
    reader.io.mem.toAhbLite3 <> mem.io.ahb
    reader.io.jump           := io.jump
    reader.io.addr           := io.addr
    reader.io.dout           <> io.dout
}

object MemStreamTest extends App {
    Config.sim.compile(MemStreamBench(NebulaCfg(), 8)).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.dout.ready #= true
        
        // Let it run for a little while
        dut.clockDomain.waitSampling(10)
    }
}