package beliefq
package relay

import spinal.core._
import spinal.lib._

object TwoMins {
  def delaysFor(deg: Int) : Int = {
    require(deg >= 2)
    var n = deg
    var delays = 0
    while(n > 1) {
      n = (n + 2) / 3
      delays += 1
    }
    delays
  }
}

case class TwoMinsResult(params: RelayParams, idWidth: Int) extends Bundle {
  import params._
  val min1, min2 = unsigned_msg_t()
  val id_min1, id_min2 = UInt(idWidth bits)
}

class TwoMins3(params: RelayParams, idWidth: Int) extends Component {
  import params._
  // IO
  val data = in port Vec.fill(3)(unsigned_msg_t())
  val ids = in port Vec.fill(3)(UInt(idWidth bits))
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(idWidth bits)
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

class TwoMins(params: RelayParams, deg: Int) extends Component {
  require(deg >= 2)
  import params._
  import TwoMins._
  val idWidth = log2Up(deg)
  val data = in port Vec.fill(deg)(unsigned_msg_t())
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(idWidth bits)

  def combineLevel(terms: Seq[TwoMinsResult]) : Seq[TwoMinsResult] = {
    val next = Seq.fill((terms.size + 2) / 3)(Reg(TwoMinsResult(params, idWidth)))
    for(g <- next.indices) {
      val cmp = new TwoMins3(params, idWidth)
      val childMin1 = Vec.fill(3)(unsigned_msg_t())
      val childMin2 = Vec.fill(3)(unsigned_msg_t())
      val childId1 = Vec.fill(3)(UInt(idWidth bits))
      val childId2 = Vec.fill(3)(UInt(idWidth bits))
      for(i <- 0 until 3) {
        val termIdx = 3 * g + i
        if(termIdx < terms.size) {
          childMin1(i) := terms(termIdx).min1
          childMin2(i) := terms(termIdx).min2
          childId1(i) := terms(termIdx).id_min1
          childId2(i) := terms(termIdx).id_min2
        } else {
          childMin1(i) := childMin1(i).maxValue
          childMin2(i) := childMin2(i).maxValue
          childId1(i) := 0
          childId2(i) := 0
        }
        cmp.data(i) := childMin1(i)
        cmp.ids(i) := i
      }

      val bestIdx = UInt(log2Up(3) bits)
      val secondIdx = UInt(log2Up(3) bits)
      bestIdx := cmp.id_min1.resized
      secondIdx := cmp.id_min2.resized

      next(g).min1 := cmp.min1
      next(g).id_min1 := childId1(bestIdx)
      when(childMin2(bestIdx) < childMin1(secondIdx)) {
        next(g).min2 := childMin2(bestIdx)
        next(g).id_min2 := childId2(bestIdx)
      } otherwise {
        next(g).min2 := childMin1(secondIdx)
        next(g).id_min2 := childId1(secondIdx)
      }
    }
    next
  }

  var terms = Seq.tabulate(deg) { i =>
    val term = TwoMinsResult(params, idWidth)
    term.min1 := data(i)
    term.min2 := term.min2.maxValue
    term.id_min1 := i
    term.id_min2 := 0
    term
  }

  val delays = delaysFor(deg)
  while(terms.size > 1) {
    terms = combineLevel(terms)
  }

  min1 := terms(0).min1
  min2 := terms(0).min2
  id_min1 := terms(0).id_min1
  id_min2 := terms(0).id_min2
}
