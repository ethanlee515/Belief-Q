package beliefq
package test

import scala.io.Source
import play.api.libs.json._

object TestDataReader extends App {
  val chkmat_serialized = Source.fromFile("./test-data/chkmat.json").mkString
  val chkmat = Json.parse(chkmat_serialized).as[Seq[Seq[Boolean]]]
  val prior_serialized = Source.fromFile("./test-data/prior.json").mkString
  val priors = Json.parse(prior_serialized).as[Seq[BigDecimal]]
  val syndromes_serialized = Source.fromFile("./test-data/syndromes.json").mkString
  val syndromes_batch = Json.parse(syndromes_serialized).as[Seq[Seq[Boolean]]]
  val ehat_bp_serialized = Source.fromFile("./test-data/ehat_bp.json").mkString
  val ehat_bp = Json.parse(ehat_bp_serialized).as[Seq[Seq[Boolean]]]
  val num_checks = chkmat.length
  val num_vars = chkmat(0).length
  val all_grid_indices = {
    for(i <- 0 until num_vars;
        j <- 0 until num_checks) yield (i, j)
  }.toSet
  val edges = all_grid_indices.filter{case (i, j) => chkmat(j)(i)}
  val geo = new TannerGraphGeometry((0 until num_vars).toSet, (0 until num_checks).toSet, edges)
  def is_converged(i : Int) : Boolean = {
    val syndromes = syndromes_batch(i)
    val corrections = ehat_bp(i)
    for(j <- 0 until num_checks) {
      val neighbor_vars = geo.get_neighboring_variables(j)
      var is_error = false
      for(v <- neighbor_vars) {
        if(corrections(v)) {
          is_error = !is_error
        }
      }
      if(is_error != syndromes(j)) {
        return false
      }
    }
    return true
  }
  var converge_count = 0
  var diverge_count = 0
  for(i <- 0 until 1000) {
    if(is_converged(i)) {
      converge_count += 1
    } else {
      diverge_count += 1
    }
  }
  println(f"converge count = ${converge_count}, diverge count = ${diverge_count}")
}
