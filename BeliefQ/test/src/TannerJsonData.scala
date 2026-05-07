package beliefq
package test

import scala.io.Source
import play.api.libs.json._

case class TannerJsonData(chkmat: Seq[Seq[Boolean]]) {
  val num_checks: Int = chkmat.length
  val num_vars: Int = if (chkmat.isEmpty) 0 else chkmat(0).length
  val var_labels: Set[Int] = (0 until num_vars).toSet
  val chk_labels: Set[Int] = (0 until num_checks).toSet
  val edges: Set[(Int, Int)] = {
    for {
      j <- 0 until num_checks
      i <- 0 until num_vars
      if chkmat(j)(i)
    } yield (i, j)
  }.toSet
}

object TannerJsonData {
  def fromFile(path: String): TannerJsonData = {
    val serialized = Source.fromFile(path).mkString
    TannerJsonData(Json.parse(serialized).as[Seq[Seq[Boolean]]])
  }
}

object StimTannerData {
  val bb144 = TannerJsonData.fromFile("./test-data/bb144_chkmat.json")
  //val colorCode = TannerJsonData.fromFile("./test-data/color_code_chkmat.json")
}
