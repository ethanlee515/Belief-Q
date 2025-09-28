package beliefq
package reference

import scala.collection.mutable

object State extends Enumeration {
  type State = Value
  val computeVToC, computeCToV, computeDecisions, checkConvergence, done = Value
}

class VanillaBP[V, F](
    var_labels: Set[V],
    factor_labels: Set[F],
    edges: Set[(V, F)],
    syndromes: Map[F, Boolean],
    log_priors: Map[V, BigDecimal]) {
  val geo = new TannerGraphGeometry(var_labels, factor_labels, edges)
  var state = State.computeCToV

  var vToCs = {
    for(e <- edges) yield {
      val (v, c) = e
      (v, c) -> log_priors(v)
    }
  }.to(mutable.Map)

  var cToVs = {
    for(e <- edges) yield {
      e -> BigDecimal(0)
    }
  }.to(mutable.Map)

  val decisions = {
    for(v <- geo.var_labels) yield { v -> false }
  }.to(mutable.Map)

  def doBP(max_iters: Int) : Option[Map[V, Boolean]] = {
    for(i <- 0 until max_iters) {
      next()
      if(isDone()) {
        return Some(decisions.toMap)
      }
    }
//    println(f"syndromes = $syndromes")
    return None
  }

  def next() = {
    state match {
      case State.computeVToC => {
        doVToC()
      }
      case State.computeCToV => {
        doCToV()
      }
      case State.computeDecisions => {
        updateDecisions()
        state = State.checkConvergence
      }
      case State.checkConvergence => {
        if(isConverged()) {
          state = State.done
        } else {
          state = State.computeVToC
        }
      }
      case State.done => {
        throw new Exception("attempting to continue a finished BP")
      }
    }
  }

  def isDone() = { state == State.done }

  def doVToC() = {
    for(v <- var_labels) {
      val neighbors = geo.get_neighboring_checks(v).toSeq
      val incoming_messages = for(c <- neighbors) yield cToVs((v, c))
      val outgoing_messages = VToC.compute(log_priors(v), incoming_messages)
      for(i <- 0 until neighbors.length) {
        val c = neighbors(i)
        vToCs((v, c)) = outgoing_messages(i)
      }
    }
    state = State.computeCToV
  }

  def doCToV() = {
    for(c <- factor_labels) {
      val neighbors = geo.get_neighboring_variables(c).toSeq
      val incoming_messages = for(v <- neighbors) yield vToCs((v, c))
      val syndrome = syndromes(c)
      val outgoing_messages = CToV.compute(syndrome, incoming_messages)
      for(i <- 0 until neighbors.length) {
        val v = neighbors(i)
        cToVs((v, c)) = outgoing_messages(i)
      }
    }
    state = State.computeDecisions
  }

  def updateDecisions() = {
    for(v <- geo.var_labels) {
      val neighbors = geo.get_neighboring_checks(v).toSeq
      val messages : Seq[BigDecimal] = {
        for(c <- neighbors) yield {
          cToVs(v, c)
        }
      }
      val sum_messages = log_priors(v) + messages.reduce(_ + _)
      decisions(v) = (sum_messages < 0)
    }
  }

  def isConverged() : Boolean = {
    for(c <- geo.chk_labels) {
      val neighbors = geo.get_neighboring_variables(c).toSeq
      val neighbor_decisions = neighbors.map(decisions(_))
      val supposed_syndrome = neighbor_decisions.reduce(_ ^ _)
      if(supposed_syndrome != syndromes(c)) {
        return false
      }
    }
    return true
  }
}
