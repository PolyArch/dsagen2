package dsagen2.mem.module

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.ChiselScalatestTester
import dsagen2.util.VcsSimulationHelper
import dsagen2.mem.module.bus.StreamReadBus
import org.scalatest.flatspec.AnyFlatSpec
import dsagen2.mem.config._
import dsagen2.mem.diplomacy._
import dsagen2.sync.config._

class StreamReadBusTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  "StreamReadBus memory response aggregator" should "work as expected" in {
    test(new StreamReadBus(new MemNodeParameters(),
      Seq(Mem2IVPParameter(new MemNodeParameters(), new IVPNodeParameters())),
      Seq(OVP2MemParameter(new OVPNodeParameters(), new MemNodeParameters())))
    (new DefaultDMAConfig)).withAnnotations(VcsDirAnnotations) { dut =>
      for(mask <- 0 to 0xff) yield {
        val data: Long = 0x0897a6b5c4d3e2f1L
        val dataSeq: Seq[Int] = Seq(0xf1, 0xe2, 0xd3, 0xc4, 0xb5, 0xa6, 0x97, 0x08)

        var nextmask: Int = mask
        val datares: collection.mutable.Seq[Int] = collection.mutable.Seq.fill(8)(0x00)
        var maskres: Int = 0
        for (idx <- 0 to 7) yield {
          maskres += nextmask % 2
          if (nextmask % 2 === 1) {
            datares(maskres - 1) = dataSeq(idx)
          }
          nextmask = nextmask / 2
        }

        maskres = (math.pow(2, maskres) - 1).toInt
        dut.strResponse.mask.get.poke(mask.U(8.W))
        dut.strResponse.readData.get.poke(data.U(64.W))
        dut.clock.step(6)
        dut.ivps.head.memValidMask.expect(maskres.U)
        dut.ivps.head.memData.zipWithIndex.foreach {
          case (res, idx) => res.expect(datares(idx).U)
        }
      }
    }
  }
}
