package beliefq
package test

import scala.io.Source
import play.api.libs.json._

object TestDataReader extends App {
  val chkmat_serialized = Source.fromFile("./test-data/chkmat.json").mkString
  val chkmat = Json.parse(chkmat_serialized).as[Seq[Seq[Boolean]]]
  val prior_serialized = Source.fromFile("./test-data/prior.json").mkString
  val priors = Json.parse(prior_serialized).as[Seq[BigDecimal]]
  val gamma_serialized = Source.fromFile("./test-data/gamma.json").mkString
  val gammas = Json.parse(gamma_serialized).as[Seq[BigDecimal]]
  val syndromes_serialized = Source.fromFile("./test-data/syndromes.json").mkString
  val syndromes = Json.parse(syndromes_serialized).as[Seq[Seq[Boolean]]]
  val ehat_bp_serialized = Source.fromFile("./test-data/ehat_bp.json").mkString
  val ehat_bp = Json.parse(ehat_bp_serialized).as[Seq[Seq[Boolean]]]
  val ehat_dmembp_serialized = Source.fromFile("./test-data/ehat_dmembp.json").mkString
  val ehat_dmembp = Json.parse(ehat_dmembp_serialized).as[Seq[Seq[Boolean]]]
}
