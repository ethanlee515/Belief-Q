package beliefq
package reference

import scala.collection.mutable

class DMemBP[V, F](
    var_labels: Set[V],
    chk_labels: Set[F],
    edges: Set[(V, F)],
    syndromes: Map[F, Boolean],
    gammas: Map[V, BigDecimal],
    log_priors: Map[V, BigDecimal]) {
  val geo = new TannerGraphGeometry(var_labels, chk_labels, edges)
  var state = State.computeCToV
  var iter0 = true
  var llr: mutable.Map[V, BigDecimal] = {
    val m = mutable.Map.empty[V, BigDecimal]
    for(v <- var_labels) {
      m(v) = BigDecimal(0)
    }
    m
  }

  var vToCs: mutable.Map[(V, F), BigDecimal] = {
    val m = mutable.Map.empty[(V, F), BigDecimal]
    for(e <- edges) {
      val (v, _) = e
      m(e) = log_priors(v)
    }
    m
  }

  var cToVs: mutable.Map[(V, F), BigDecimal] = {
    val m = mutable.Map.empty[(V, F), BigDecimal]
    for(e <- edges) {
      m(e) = BigDecimal(0)
    }
    m
  }

  val decisions: mutable.Map[V, Boolean] = {
    val m = mutable.Map.empty[V, Boolean]
    for(v <- geo.var_labels) {
      m(v) = false
    }
    m
  }

  def doBP(max_iters: Int) : Option[Map[V, Boolean]] = {
    for(_ <- 0 until max_iters) {
      step()
      if(isDone()) {
        return Some(decisions.toMap)
      }
    }
    return None
  }

  def step() = {
    state match {
      case State.computeVToC => {
        doVToC()
        state = State.computeCToV
      }
      case State.computeCToV => {
        doCToV()
        state = State.computeDecisions
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
      for(i <- 0 until neighbors.length) {
        val c = neighbors(i)
        vToCs((v, c)) = llr(v) - cToVs((v, c))
      }
    }
  }

  def doCToV() = {
    for(c <- chk_labels) {
      val neighbors = geo.get_neighboring_variables(c).toSeq
      val incoming_messages = for(v <- neighbors) yield vToCs((v, c))
      val outgoing_messages = CToV.compute(syndromes(c), incoming_messages)
      for(i <- 0 until neighbors.length) {
        val v = neighbors(i)
        cToVs((v, c)) = outgoing_messages(i)
      }
    }
  }

  def updateDecisions() = {
    for(v <- geo.var_labels) {
      val bias = if(iter0) {
        log_priors(v)
      } else {
        (1 - gammas(v)) * log_priors(v) + gammas(v) * llr(v)
      }
      val neighbors = geo.get_neighboring_checks(v).toSeq
      val messages = {
        for(c <- neighbors) yield {
          cToVs((v, c))
        }
      }
      val marginal = bias + messages.reduce(_ + _)
      llr(v) = marginal
      decisions(v) = (marginal < 0)
    }
    iter0 = false
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
