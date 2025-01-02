package RestoringDivider

import chisel3._
import chisel3.util._
import chisel3.util.Decoupled._

class ArithBundle(len: Int = 32) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val out = DecoupledIO(Output(UInt((len * 2).W)))
  val sign = Input(Bool())
}

