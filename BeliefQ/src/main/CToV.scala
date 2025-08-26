package beliefq

import spinal.core._
import spinal.lib._

case class CToVInputs(params: BeliefQParams, deg: Int) extends Bundle {
  import params._
  val syndrome = Bool()
  val messages = Vec.fill(deg)(message_t())
}

class CToV(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val inputs = in port Flow(CToVInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(message_t()))
  val delays = 2 * deg + 1
  // states
  object State extends SpinalEnum {
    val idle, forward, backward, done = newElement()
  }
  val state = RegInit(State.idle)
  val ctr = Reg(UInt(log2Up(deg) bits)) init(0)
  val abs_messages = Vec.fill(deg)(Reg(message_t))
  val is_negatives = Vec.fill(deg)(Reg(Bool()))
  val result = Vec.fill(deg)(Reg(message_t))
  output.valid := (state === State.done)
  output.payload := result
  val min1_valid = Reg(Bool())
  val min2_valid = Reg(Bool())
  val min1 = Reg(message_t())
  val min2 = Reg(message_t())
  val min1_idx = Reg(UInt(log2Up(deg) bits))
  val min2_idx = Reg(UInt(log2Up(deg) bits))
  val sign_parity = Reg(Bool())
  switch(state) {
    is(State.idle) {
      when(inputs.valid) {
        state := State.forward
        ctr := 0
        for(i <- 0 until deg) {
          val m = inputs.payload.messages(i)
          val is_negative = m.isNegative
          is_negatives(i) := is_negative
          val minus_m = message_t()
          minus_m := (-m).truncated
          abs_messages(i) := is_negative ? minus_m | m
        }
        sign_parity := inputs.payload.syndrome
        min1_valid := False
        min2_valid := False
      }
    }
    is(State.forward) {
      val m = abs_messages(ctr)
      when((!min1_valid) || m < min1) {
        min2 := min1
        min2_idx := min1_idx
        min2_valid := min1_valid
        min1 := m
        min1_idx := ctr
        min1_valid := True
      } elsewhen((!min2_valid) || m < min2) {
        min2 := m
        min2_idx := ctr
        min2_valid := True
      }
      val s = is_negatives(ctr)
      sign_parity := sign_parity ^ s
      when(ctr === deg - 1) {
        state := State.backward
      } otherwise {
        ctr := ctr + 1
      }
    }
    is(State.backward) {
      val ri_sgn = sign_parity ^ is_negatives(ctr)
      val ri = (ctr === min1_idx) ? min2 | min1
      val minus_ri = message_t()
      minus_ri := (-ri).truncated
      result(ctr) := ri_sgn ? minus_ri | ri
      when(ctr === 0) {
        state := State.done
      } otherwise {
        ctr := ctr - 1
      }
    }
    is(State.done) {
      state := State.idle
    }
  }
}
