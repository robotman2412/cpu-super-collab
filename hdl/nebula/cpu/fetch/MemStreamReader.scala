package nebula.cpu.fetch

// Copyright © 2024, Julian Scheffers, see LICENSE for info

import nebula.cpu._
import nebula.cpu.mem._
import spinal.core._
import spinal.lib._
import _root_.nebula.cpu.mem.NebulaMemBus.Mode



/** Packet of memory stream data. */
case class MemStreamPacket(cfg: NebulaCfg, width: Int) extends Bundle {
    /** Address of fetched word, always naturally aligned. */
    val addr    = UInt(cfg.vaddrWidth bits)
    /** Fetched word. */
    val data    = Bits(width bits)
    /** Access generated trap. */
    val trap    = Bool()
    /** Generated trap number. */
    val cause   = UInt(5 bits)
}

/**
 * Reads words of memory and outputs them in a stream.
 * When `jump` is raised, invalidate the memory access and start over at `addr`.
 * If provided a misaligned address, the lower bits are truncated.
 */
case class MemStreamReader(cfg: NebulaCfg, width: Int, entrypoint: BigInt, isCode: Boolean = true) extends Component {
    assert(width >= 8 && width <= 1024, "Width must be at between 8 and 1024 bits")
    assert((width & (width-1)) == 0, "Width must be a power of two")
    val io = new Bundle {
        /** Memory interface. */
        val mem  = master port NebulaMemBus(cfg, true)
        /** Switch address to fetch from. */
        val jump = in     port Bool()
        /** The address to jump to. */
        val addr = in     port UInt(cfg.vaddrWidth bits)
        /** Output data stream. */
        val dout = master port Stream(MemStreamPacket(cfg, width))
    }
    /** Number of sub-word address bits for the purpose of fetching. */
    val subWord = log2Up(width/8)
    
    /** Current address. */
    val pc      = RegInit(U(entrypoint >> subWord << subWord, cfg.vaddrWidth bits))
    /** Whether the buffer contains any data. */
    val hasBuf  = Bool()
    /** Buffer used in case the next stage isn't ready. */
    val buf     = Bits(width bits)
    /** Whether a memory request was made last cycle. */
    val req     = Bool()
    
    // Request logic.
    io.mem.mode := Mode.IDLE
    when (io.jump) {
        pc := io.addr
    } elsewhen ((!req || io.mem.ready) && (!hasBuf || io.dout.ready)) {
        pc := pc + (1 << subWord)
        io.mem.mode := (if (isCode) Mode.EXEC else Mode.READ)
    }
    io.mem.asize := log2Up(width / 8)
    io.mem.addr  := pc
}
