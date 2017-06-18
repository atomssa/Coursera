package observatory

import java.util.Calendar

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

  test("create width height array 2x2") {
    val a = Visualization.whArray(2,2)
    assert(a === Array((0,0),(1,0),(0,1),(1,1)))
  }
  test("create width height array 3x2") {
    val a = Visualization.whArray(3,2)
    assert(a === Array((0,0),(1,0),(2,0),(0,1),(1,1),(2,1)))
  }

  val colorScale = Iterable(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))

  test("Color lower than lower bound") {
    val i = Visualization.interpolateColor(colorScale, -70)
    assert(i === Color(0,0,0))
  }

  test("Color higher than upper bound") {
    val i = Visualization.interpolateColor(colorScale, 70)
    assert(i === Color(255,255,255))
  }

  test("Color somewhere in the middle") {
    val i = Visualization.interpolateColor(colorScale, 6)
    assert(i === Color(128,255,128))
  }

  test("pixel to location conversion top left") {
    assert(Visualization.pixelToLoc(0,0,360,180) === Location(90,-180))
  }
  test("pixel to location conversion top right") {
    assert(Visualization.pixelToLoc(360,0,360,180) === Location(90, 180))
  }
  test("pixel to location conversion bottom left") {
    assert(Visualization.pixelToLoc(0,180,360,180) === Location(-90, -180))
  }
  test("pixel to location conversion bottom right") {
    assert(Visualization.pixelToLoc(360,180,360,180) === Location(-90, 180))
  }
  test("pixel to location conversion origin") {
    assert(Visualization.pixelToLoc(180,90,360,180) === Location(0,0))
  }

  test("pixel to location some random location") {
    assert(Visualization.pixelToLoc(153,180,360,180) === Location(-90,-27))
  }

  test("coursera stuff") {
    val colorScale2 = Iterable(
      (100.0, Color(0, 0, 255)),
      (-100.0, Color(255, 0, 0)))
    val temps = Iterable(
      (Location(45,-90), -100.0),
      (Location(-45,0), 100.0))
    val img = Visualization.visualize(temps,colorScale2)
    val col = Color(img.pixel(153 , 179).red, img.pixel(153 , 179).green, img.pixel(153 , 179).blue)
    assert(col === Color(25,0,230))
  }

  def curTime: String = {
    val now = Calendar.getInstance()
    val hh = now.get(Calendar.HOUR)
    val mm = now.get(Calendar.MINUTE)
    val ss = now.get(Calendar.SECOND)
    val ms = now.get(Calendar.MILLISECOND)
    s"$hh:$mm:$ss.$ms"
  }

  /*
  test("create image file from 2005 data") {
    println(s"test($curTime): Importing data...")
    val records = Extraction.locateTemperatures(2005, "/test/visu_stations.csv", "/test/visu_2005.csv")
    val avgTemp = Extraction.locationYearlyAverageRecords(records)
    println(s"test($curTime): Visualizing data...")
    val img = Visualization.visualize(avgTemp,colorScale)
    println(s"test($curTime): Saving image file...")
    img.output(new java.io.File("/Users/tujuba/Desktop/youpi.png"))
    println(s"test($curTime): Done")
    assert(img.width === Visualization.width && img.height === Visualization.height)
  }
  */

}
