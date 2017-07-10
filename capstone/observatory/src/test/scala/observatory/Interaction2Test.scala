package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Interaction2Test extends FunSuite with Checkers {

  test("bounds") {
    val yb = Interaction2.yearBounds(Signal(Interaction2.availableLayers.head))
    assert(yb().start === 1975 && yb().end === 2015)
  }

  test("year selection ") {
    val ys = Interaction2.yearSelection(Signal(Interaction2.availableLayers.head), Signal(0))
    assert(ys() === 1975)
  }

  test("year selection 2") {
    val sv = Signal(2017)
    val ys = Interaction2.yearSelection(Signal(Interaction2.availableLayers.head), sv)
    assert(ys() === 2015)
  }

}
