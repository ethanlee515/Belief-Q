package beliefq

import spinal.core._
import spinal.lib._

case class CToVInputs(params: BeliefQParams, deg: Int) extends Bundle {
  import params._
  val syndrome = Bool()
  val messages = Vec.fill(deg)(message_t())
}

class TwoMins3(params: BeliefQParams) extends Component {
  import params._
  // IO
  val data = in port Vec.fill(3)(unsigned_msg_t())
  val ids = in port Vec.fill(3)(UInt(3 bits))
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(3 bits)
  // logic
  val lt01 = data(0) < data(1)
  val lt02 = data(0) < data(2)
  val lt12 = data(1) < data(2)
  when(lt01 && lt02) {
    min1 := data(0)
    id_min1 := ids(0)
    when(lt12) {
      min2 := data(1)
      id_min2 := ids(1)
    } otherwise {
      min2 := data(2)
      id_min2 := ids(2)
    }
  } elsewhen(!lt01 && lt12) {
    min1 := data(1)
    id_min1 := ids(1)
    when(lt02) {
      min2 := data(0)
      id_min2 := ids(0)
    } otherwise {
      min2 := data(2)
      id_min2 := ids(2)
    }
  } otherwise {
    min1 := data(2)
    id_min1 := ids(2)
    when(lt01) {
      min2 := data(0)
      id_min2 := ids(0)
    } otherwise {
      min2 := data(1)
      id_min2 := ids(1)
    }
  }
}

class TwoMins6(params: BeliefQParams) extends Component {
  import params._
  // IO
  val data = in port Vec.fill(6)(unsigned_msg_t())
  val ids = in port Vec.fill(6)(UInt(3 bits))
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(3 bits)
  // logic
  val left, right = new TwoMins3(params)
  left.data := Vec(data.slice(0, 3))
  left.ids := Vec(ids.slice(0, 3))
  right.data := Vec(data.slice(3, 6))
  right.ids := Vec(ids.slice(3, 6))
  val left_min1 = RegNext(left.min1)
  val left_min2 = RegNext(left.min2)
  val left_id1 = RegNext(left.id_min1)
  val left_id2 = RegNext(left.id_min2)
  val right_min1 = RegNext(right.min1)
  val right_min2 = RegNext(right.min2)
  val right_id1 = RegNext(right.id_min1)
  val right_id2 = RegNext(right.id_min2)
  when(left_min1 < right_min1) {
    min1 := left_min1
    id_min1 := left_id1
    when(left_min2 < right_min1) {
      min2 := left_min2
      id_min2 := left_id2
    } otherwise {
      min2 := right_min1
      id_min2 := right_id1
    }
  } otherwise { // right_min1 < left_min1
    min1 := right_min1
    id_min1 := right_id1
    when(right_min2 < left_min1) {
      min2 := right_min2
      id_min2 := right_id2
    } otherwise {
      min2 := left_min1
      id_min2 := left_id1
    }
  }
}

/*
class TwoMins(deg: Int) extends Component {

}
*/

class CToV(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val inputs = in port Flow(CToVInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(message_t()))
  val delays = 2 * deg + 1
  // states
  object LocalState extends SpinalEnum {
    val idle, forward, backward, done = newElement()
  }
  val state = RegInit(LocalState.idle)
  val ctr = Reg(UInt(log2Up(deg) bits)) init(0)
  val abs_messages = Vec.fill(deg)(Reg(message_t))
  val is_negatives = Vec.fill(deg)(Reg(Bool()))
  val result = Vec.fill(deg)(Reg(message_t))
  output.valid := (state === LocalState.done)
  output.payload := result
  val min1_valid = Reg(Bool())
  val min2_valid = Reg(Bool())
  val min1 = Reg(message_t())
  val min2 = Reg(message_t())
  val min1_idx = Reg(UInt(log2Up(deg) bits))
  val min2_idx = Reg(UInt(log2Up(deg) bits))
  val sign_parity = Reg(Bool())
  switch(state) {
    is(LocalState.idle) {
      when(inputs.valid) {
        state := LocalState.forward
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
    is(LocalState.forward) {
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
        state := LocalState.backward
      } otherwise {
        ctr := ctr + 1
      }
    }
    is(LocalState.backward) {
      val ri_sgn = sign_parity ^ is_negatives(ctr)
      val ri = (ctr === min1_idx) ? min2 | min1
      val minus_ri = message_t()
      minus_ri := (-ri).truncated
      result(ctr) := ri_sgn ? minus_ri | ri
      when(ctr === 0) {
        state := LocalState.done
      } otherwise {
        ctr := ctr - 1
      }
    }
    is(LocalState.done) {
      state := LocalState.idle
    }
  }
}
