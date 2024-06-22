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
    assert(width >= 8 && width <= 1024, "Width must be between 8 and 1024 bits")
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
    /** Address of memory response. */
    val oldpc   = Reg(UInt(cfg.vaddrWidth bits))
    /** Whether the buffer contains any data. */
    val hasBuf  = RegInit(False)
    /** Buffer used in case the next stage isn't ready. */
    val buf     = Reg(MemStreamPacket(cfg, width))
    /** Whether a memory request was made last cycle. */
    val req     = RegInit(False)
    
    // Request logic.
    io.mem.mode := Mode.IDLE
    io.dout.payload.addr.setAsReg()
    when (io.jump) {
        pc          := io.addr
        req         := False
    } elsewhen ((!req || io.mem.ready) && (!hasBuf || io.dout.ready)) {
        oldpc       := pc
        pc          := pc + (1 << subWord)
        req         := True
        io.mem.mode := (if (isCode) Mode.EXEC else Mode.READ)
    } elsewhen (hasBuf && !io.dout.ready) {
        req         := False
    }
    io.mem.asize := log2Up(width / 8)
    io.mem.addr  := pc
    
    // Stream logic.
    when (io.jump) {
        // Jump; discard old data.
        hasBuf                  := False
        io.dout.valid           := False
        io.dout.payload.assignDontCare()
    } elsewhen (hasBuf) {
        // Buffered data.
        io.dout.valid           := True
        io.dout.payload         := buf
        hasBuf                  := !io.dout.ready
        when (io.dout.ready) {
            buf.addr            := oldpc
            buf.data            := io.mem.rdata(width-1 downto 0)
            buf.trap            := io.mem.trap
            buf.cause           := io.mem.cause
        }
    } otherwise {
        // Unbuffered data.
        io.dout.valid           := io.mem.ready && req
        io.dout.payload.addr    := oldpc
        io.dout.payload.data    := io.mem.rdata(width-1 downto 0)
        io.dout.payload.trap    := io.mem.trap
        io.dout.payload.cause   := io.mem.cause
        buf                     := io.dout.payload
        hasBuf                  := !io.dout.ready
    }
}
