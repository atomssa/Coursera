package observatory

import observatory.Manipulation._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {
  test("transformations") {
    val g = (for (i <- (0 to 64800).toParArray) yield {
      assert(t(tiLat(i),tiLon(i)) === i, s"problem at $i")
    }).toArray

  }
}