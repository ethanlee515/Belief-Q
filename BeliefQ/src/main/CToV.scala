package beliefq

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

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
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(3 bits)
  // logic
  val left, right = new TwoMins3(params)
  for(i <- 0 until 3) {
    left.ids(i) := i
    right.ids(i) := i + 3
  }
  left.data := Vec(data.slice(0, 3))
  right.data := Vec(data.slice(3, 6))
  val left_min1 = RegNext(left.min1)
  val left_min2 = RegNext(left.min2)
  val left_id1 = RegNext(left.id_min1)
  val left_id2 = RegNext(left.id_min2)
  val right_min1 = RegNext(right.min1)
  val right_min2 = RegNext(right.min2)
  val right_id1 = RegNext(right.id_min1)
  val right_id2 = RegNext(right.id_min2)
  val is_left = RegNext(left_min1 < right_min1)
  when(is_left) {
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

class TwoMins(params: BeliefQParams, deg: Int) extends Component {
  require(deg <= 6)
  import params._
  val data = in port Vec.fill(deg)(unsigned_msg_t())
  val min6 = new TwoMins6(params)
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(3 bits)
  for(i <- 0 until deg) {
    min6.data(i) := data(i)
  }
  for(i <- deg until 6) {
    min6.data(i) := min6.data(i).maxValue
  }
  min1 := min6.min1
  min2 := min6.min2
  id_min1 := min6.id_min1
  id_min2 := min6.id_min2
}

class CToV(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val inputs = in port Flow(CToVInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(message_t()))
  /* -- logic -- */
  val parsingStage = Node()
  val a1 = new parsingStage.Area {
    val abs_m = Vec.fill(deg)(unsigned_msg_t())
    val is_neg = Vec.fill(deg)(Bool())
    val s = Bool()
    for(i <- 0 until deg) {
      val m = inputs.payload.messages(i)
      val mt = unsigned_msg_t()
      mt := m.truncated
      val is_negative = m.isNegative
      is_neg(i) := is_negative
      val minus_m = unsigned_msg_t()
      minus_m := (-m).truncated
      abs_m(i) := (is_negative ? minus_m | mt)
    }
    s := inputs.payload.syndrome
    val abs_messages = insert(abs_m)
    val is_negatives = insert(is_neg)
    val sign_parity = insert(s)
  }
  val twoMinsStage1 = Node()
  val a2 = new twoMinsStage1.Area {
    val twomins = new TwoMins(params, deg)
    twomins.data := a1.abs_messages
    val xor_signs = insert(
      a1.sign_parity ^ a1.is_negatives.asBits.xorR)
  }
  val twoMinsStageExtra = Node()
  val twoMinsStage2 = Node()
  val a3 = new twoMinsStage2.Area {
    val min1 = insert(a2.twomins.min1) 
    val min2 = insert(a2.twomins.min2) 
    val id_min1 = insert(a2.twomins.id_min1) 
    val id_min2 = insert(a2.twomins.id_min2)
    val res_signs = Vec.fill(deg)(Bool())
    for(i <- 0 until deg) {
      res_signs(i) := a2.xor_signs ^ a1.is_negatives(i)
    }
    val result_signs = insert(res_signs)
  }
  val outputStage = Node()
  val a4 = new outputStage.Area {
    for(i <- 0 until deg) {
      val ri = message_t()
      when(a3.id_min1 === U(i)) {
        ri := a3.min2
      } otherwise {
        ri := a3.min1
      }
      val minus_ri = message_t()
      minus_ri := (-ri).truncated
      output.payload(i) := a3.result_signs(i) ? minus_ri | ri
    }
    output.valid := isValid
  }
  parsingStage.valid := inputs.valid
  val pipeline = Builder(List(
    StageLink(parsingStage, twoMinsStage1),
    StageLink(twoMinsStage1, twoMinsStageExtra),
    StageLink(twoMinsStageExtra, twoMinsStage2),
    StageLink(twoMinsStage2, outputStage)))
  val delays = 4
}
