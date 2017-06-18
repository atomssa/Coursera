package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("distance between two opposite points") {
    val l1 = Location(90,0)
    val l2 = Location(-90,0)
    val d = Visualization.dist(l1, l2)
    assert(d === Math.PI * Visualization.earthRadius)
  }

  test("distance between pole and equator") {
    val l1 = Location(90,0)
    val l2 = Location(0,0)
    val d = Visualization.dist(l1, l2)
    assert(d === Math.PI * Visualization.earthRadius/2.0)
  }

  test("distance between opposite points on equator") {
    val l1 = Location(0,0)
    val l2 = Location(0,180)
    val d = Visualization.dist(l1, l2)
    assert(d === Math.PI * Visualization.earthRadius)
  }

  val c = Color(1,2,3)
  val colorScale = Iterable(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))

  test("create image file from 2000 data") {
    val records = Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv")
    val avgTemp = Extraction.locationYearlyAverageRecords(records)
    val img = Visualization.visualize(avgTemp,colorScale)
    assert(img.width === Visualization.width && img.height === Visualization.height)
  }
}
