package beliefq
package relay

import spinal.core._
import spinal.lib._

object TwoMins {
  val MaxDeg = 9
  val IdWidth = log2Up(MaxDeg)
}

class TwoMins3(params: RelayParams) extends Component {
  import params._
  import TwoMins._
  // IO
  val data = in port Vec.fill(3)(unsigned_msg_t())
  val ids = in port Vec.fill(3)(UInt(IdWidth bits))
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(IdWidth bits)
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

class TwoMins6(params: RelayParams) extends Component {
  import params._
  import TwoMins._
  // IO
  val data = in port Vec.fill(6)(unsigned_msg_t())
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(IdWidth bits)
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

class TwoMins9(params: RelayParams) extends Component {
  import params._
  import TwoMins._
  // IO
  val data = in port Vec.fill(9)(unsigned_msg_t())
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(IdWidth bits)
  // logic
  val groups = Seq.fill(3)(new TwoMins3(params))
  for(g <- 0 until 3) {
    for(i <- 0 until 3) {
      groups(g).data(i) := data(3 * g + i)
      groups(g).ids(i) := 3 * g + i
    }
  }
  val group_min1 = Vec(groups.map(g => RegNext(g.min1)))
  val group_min2 = Vec(groups.map(g => RegNext(g.min2)))
  val group_id1 = Vec(groups.map(g => RegNext(g.id_min1)))
  val group_id2 = Vec(groups.map(g => RegNext(g.id_min2)))

  val bestGroups = new TwoMins3(params)
  bestGroups.data := group_min1
  for(i <- 0 until 3) {
    bestGroups.ids(i) := i
  }

  val bestGroup = bestGroups.id_min1
  val secondGroup = bestGroups.id_min2
  val bestGroupIndex = UInt(log2Up(3) bits)
  val secondGroupIndex = UInt(log2Up(3) bits)
  bestGroupIndex := bestGroup.resized
  secondGroupIndex := secondGroup.resized
  val secondFromBestGroup = group_min2(bestGroupIndex)
  val firstFromSecondGroup = group_min1(secondGroupIndex)

  min1 := bestGroups.min1
  id_min1 := group_id1(bestGroupIndex)
  when(secondFromBestGroup < firstFromSecondGroup) {
    min2 := secondFromBestGroup
    id_min2 := group_id2(bestGroupIndex)
  } otherwise {
    min2 := firstFromSecondGroup
    id_min2 := group_id1(secondGroupIndex)
  }
}

class TwoMins(params: RelayParams, deg: Int) extends Component {
  require(deg <= TwoMins.MaxDeg)
  import params._
  import TwoMins._
  val data = in port Vec.fill(deg)(unsigned_msg_t())
  val min9 = new TwoMins9(params)
  val min1, min2 = out port unsigned_msg_t()
  val id_min1, id_min2 = out port UInt(IdWidth bits)
  for(i <- 0 until deg) {
    min9.data(i) := data(i)
  }
  for(i <- deg until TwoMins.MaxDeg) {
    min9.data(i) := min9.data(i).maxValue
  }
  min1 := min9.min1
  min2 := min9.min2
  id_min1 := min9.id_min1
  id_min2 := min9.id_min2
}
