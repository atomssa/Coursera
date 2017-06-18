package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  /*
  val iter = Extraction.locateTemperatures(2000, "/stations.csv", "/2000.csv")
  val agg = Extraction.locationYearlyAverageRecords(iter)

  test("resource path") {
    val path = Extraction.fsPath("/stations.csv")
    assert(path.nonEmpty, "path is empty")
  }

  test("number of temp records") {
    println(s"number of temp records with valid station id = ${iter.size}")
    assert(iter.nonEmpty, s"incorrect number of temp records with valid station id = ${iter.size}")
  }

  test("number of stations") {
    println(s"number of stations with yearly average = ${agg.size}")
    assert(agg.nonEmpty, s"incorrect number of stations with average = ${agg.size}")
  }
  */

  /*
  test("check join with null values") {
    val records = Extraction.locateTemperatures(2000, "/test/stations.csv", "/test/2000.csv")
    val agg = Extraction.locationYearlyAverageRecords(records)
    assert(agg.size === 5)
  }
  */

}