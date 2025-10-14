package beliefq

import spinal.core._
import spinal.lib._

class TannerGraphGeometry[V, C](
    val var_labels: Set[V],
    val chk_labels: Set[C],
    val edge_labels: Set[(V, C)]) {
  def get_neighboring_checks(variable: V) : Set[C] = {
    edge_labels.collect{case (v, f) if (v == variable) => f}
  }.toSet
  def get_neighboring_variables(check: C) : Set[V] = {
    edge_labels.collect{case (v, f) if (f == check) => v}
  }.toSet
  def deg_var(v: V) : Int = {
    get_neighboring_checks(v).size
  }
  def deg_check(c: C) : Int = {
    get_neighboring_variables(c).size
  }
}