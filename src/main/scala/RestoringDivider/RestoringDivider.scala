package RestoringDivider

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

class RestoringDivider(len: Int = 32) extends Module {
  val io = IO(new ArithBundle(len))

  def abs(a: UInt, sign: Bool): (Bool, UInt) = {
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val s_idle :: s_log2 :: s_shift :: s_compute :: s_finish :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val newReq = (state === s_idle) && io.in.fire

  val (dividend, divisor) = (io.in.bits(0), io.in.bits(1))
  val divBy0 = divisor === 0.U(len.W)

  val shiftReg = Reg(UInt((1 + len * 2).W))
  val hi = shiftReg(len * 2, len)
  val lo = shiftReg(len - 1, 0)

  val (dividendSign, dividendVal) = abs(dividend, io.sign)
  val (divisorSign, divisorVal) = abs(divisor, io.sign)
  val dividendSignReg = RegEnable(dividendSign, newReq)
  val quotientSignReg = RegEnable((dividendSign ^ divisorSign) && !divBy0, newReq)
  val divisorReg = RegEnable(divisorVal, newReq)
  val dividendValx2Reg = RegEnable(Cat(dividendVal, "b0".U), newReq)

  val cnt = Counter(len)
  when (newReq) {
    state := s_log2
  } .elsewhen (state === s_log2) {
    val canSkipShift = (len.U | Log2(divisorReg)) - Log2(dividendValx2Reg)
    cnt.value := Mux(divBy0, 0.U, Mux(canSkipShift >= (len-1).U, (len-1).U, canSkipShift))
    state := s_shift
  } .elsewhen (state === s_shift) {
    shiftReg := dividendValx2Reg << cnt.value
    state := s_compute
  } .elsewhen (state === s_compute) {
    val enough = hi.asUInt >= divisorReg.asUInt
    shiftReg := Cat(Mux(enough, hi - divisorReg, hi)(len - 1, 0), lo, enough)
    cnt.inc()
    when (cnt.value === (len-1).U) { state := s_finish }
  } .elsewhen (state === s_finish) {
    state := s_idle
  }

  val r = hi(len, 1)
  val Quotient = Mux(quotientSignReg, -lo, lo)
  val Remainder = Mux(dividendSignReg, -r, r)
  io.out.bits := Cat(Remainder, Quotient)

  io.out.valid := state === s_finish
  io.in.ready := (state === s_idle)
}

object RestoringDividerApp extends App {
  ChiselStage.emitSystemVerilogFile(
    new RestoringDivider(32),
    firtoolOpts = Array("-o", "./build/systemverilog/RestoringDivider.sv", 
    "-disable-all-randomization", 
    "-strip-debug-info")
  )
}