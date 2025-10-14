package beliefq

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

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
  val twoMinsStageExtra1 = Node()
  val twoMinsStageExtra2 = Node()
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
  val rsSelectionStage = Node()
  val a4 = new rsSelectionStage.Area {
    val rv = Vec.fill(deg)(message_t())
    for(i <- 0 until deg) {
      when(a3.id_min1 === U(i)) {
        rv(i) := a3.min2
      } otherwise {
        rv(i) := a3.min1
      }
    }
    val rs = insert(rv)
  }
  val outputStage = Node()
  val a5 = new outputStage.Area {
    for(i <- 0 until deg) {
      val minus_ri = message_t()
      minus_ri := (-a4.rs(i)).truncated
      output.payload(i) := a3.result_signs(i) ? minus_ri | a4.rs(i)
    }
    output.valid := isValid
  }
  parsingStage.valid := inputs.valid
  val pipeline = Builder(List(
    StageLink(parsingStage, twoMinsStage1),
    StageLink(twoMinsStage1, twoMinsStageExtra1),
    StageLink(twoMinsStageExtra1, twoMinsStageExtra2),
    StageLink(twoMinsStageExtra2, twoMinsStage2),
    StageLink(twoMinsStage2, rsSelectionStage),
    StageLink(rsSelectionStage, outputStage)))
  val delays = 6
}
