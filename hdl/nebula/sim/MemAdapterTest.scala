package nebula.sim

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import nebula._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import nebula.cpu.mem._
import nebula.cpu._

case class MemAdapterBench(cfg: NebulaCfg) extends Component {
    val io = new Bundle {
        val nmem = slave(NebulaMemBus(cfg))
        val ahb3 = nmem.toAhbLite3
    }
}

object MemAdapterTest extends App {
    Config.sim.compile(MemAdapterBench(NebulaCfg())).doSim(this.getClass.getSimpleName) { dut =>
        // Fork a process to generate the reset and the clock on the dut
        dut.clockDomain.forkStimulus(period = 10)
        val nmem = dut.io.nmem
        val ahb3 = dut.io.ahb3
        ahb3.HREADY #= true

        // Read (uint32_t *) 0x0100
        nmem.mode   #= NebulaMemBus.Mode.READ
        nmem.addr   #= 0x0100
        nmem.asize  #= 2
        nmem.wdata.randomize()
        dut.clockDomain.waitSampling()
        // Response: 0xdeadbeef
        ahb3.HREADY #= true
        ahb3.HRESP  #= false
        ahb3.HRDATA #= 0xdeadbeefl
        
        // Write (uint16_t *) 0x1f02 <= 0xcafe
        nmem.mode   #= NebulaMemBus.Mode.WRITE
        nmem.addr   #= 0x1f02
        nmem.asize  #= 1
        nmem.wdata  #= 0xcafe
        dut.clockDomain.waitSampling()
        // Response: Stall.
        ahb3.HREADY #= false
        ahb3.HRESP  #= false
        ahb3.HRDATA.randomize()
        
        // Queued write: (uint16_t *) 0x1f04 <= 0xbabe
        nmem.mode   #= NebulaMemBus.Mode.WRITE
        nmem.addr   #= 0x1f04
        nmem.asize  #= 1
        nmem.wdata  #= 0xbabe
        dut.clockDomain.waitSampling()
        // Response: Ready (after stalled write).
        ahb3.HREADY #= true
        ahb3.HRESP  #= false
        ahb3.HRDATA.randomize()
        
        // Stalled.
        dut.clockDomain.waitSampling()
        // Response: Ready (after queued write).
        ahb3.HREADY #= true
        ahb3.HRESP  #= false
        ahb3.HRDATA.randomize()
        
        // Idle.
        nmem.mode   #= NebulaMemBus.Mode.IDLE
        nmem.addr.randomize()
        nmem.asize.randomize()
        nmem.wdata.randomize()
        dut.clockDomain.waitSampling()
        // Response: Ready.
        ahb3.HREADY #= true
        ahb3.HRESP  #= false
        ahb3.HRDATA.randomize()
    }
}