package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.{DataTypes, StructField, StructType}

import org.apache.log4j.{Level, Logger}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val tempDf: DataFrame = spark.read.schema(tempRecordSchema).option("header", false).csv(fsPath(temperaturesFile))
    val stationDf: DataFrame = spark.read.schema(stationSchema).option("header", false).csv(fsPath(stationsFile))

    val tempDs = tempDf.as[TempRecord].where(tempDf("stn").isNotNull and tempDf("wban").isNotNull and tempDf("month").isNotNull and tempDf("day").isNotNull and tempDf("temp").isNotNull)
    val stationDs = stationDf.as[Station].where(stationDf("s_stn").isNotNull and stationDf("s_wban").isNotNull and stationDf("lat").isNotNull and stationDf("lon").isNotNull)

    println("@@@ stations dataset: ")
    stationDs.show(20)

    println("@@@ temperatures dataset: ")
    tempDs.show(20)

    println("@@@  joining stations and temperatures")
    val tempStationDs = tempDs.join(stationDs, tempDs("stn") === stationDs("s_stn") and tempDs("wban") === stationDs("s_wban")).as[TempStation]

    println("@@@ join result dataset: ")
    tempStationDs.show(20)

    def convert(ts: TempStation): (LocalDate, Location, Double) = {
      val localDate = LocalDate.of(year, ts.month, ts.day)
      val location = Location(ts.lat, ts.lon)
      (localDate, location, (ts.temp - 32.0) * 5.0 / 9.0 )
    }

    tempStationDs.collect.map(convert)

  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val grpByLoc = records.groupBy( a => a._2)
    def avgTemp(it: Iterable[(LocalDate, Location, Double)]): Double = {
      val temps: Iterable[Double] = it.map(_._3)
      temps.sum / temps.size
    }
    val ans = grpByLoc.map( a => (a._1, avgTemp(a._2)))
    ans.take(10).foreach( println(_) )
    ans
  }

  final case class TempRecord(stn: Int, wban: Int, month: Int, day: Int, temp: Double)
  final case class Station(s_stn: Int, s_wban: Int, lat: Double, lon: Double)
  final case class TempStation(stn: Int, wban: Int, month: Int, day: Int, temp: Double, s_stn: Int, s_wban: Int, lat: Double, lon: Double)

  val t_stn       = StructField("stn",         DataTypes.IntegerType, nullable = true)
  val t_wban      = StructField("wban",        DataTypes.IntegerType, nullable = true)
  val s_stn       = StructField("s_stn",       DataTypes.IntegerType, nullable = true)
  val s_wban      = StructField("s_wban",      DataTypes.IntegerType, nullable = true)

  val month     = StructField("month",     DataTypes.IntegerType, nullable = true)
  val day       = StructField("day",       DataTypes.IntegerType, nullable = true)
  val temp      = StructField("temp",      DataTypes.DoubleType, nullable = true)
  val lat       = StructField("lat",       DataTypes.DoubleType, nullable = true)
  val lon       = StructField("lon",       DataTypes.DoubleType, nullable = true)

  val tempRecordFields = Array(t_stn, t_wban, month, day, temp)
  val tempRecordSchema = StructType(tempRecordFields)

  val stationFields = Array(s_stn, s_wban, lat, lon)
  val stationSchema = StructType(stationFields)


/*
    [Test Description] [#1 - Data extraction] weather stations are identified by the composite (STN, WBAN)
    [Observed Error] Set((2000-01-01,Location(1.0,-1.0),50.0), (2000-01-05,Location(5.0,-5.0),50.0)) had size 2 instead of expected size 5
    [Lost Points] 3

  [Test Description] [#1 - Data extraction] temperatures are located
    [Observed Error] (2000-01-01,Location(1.0,-1.0),50.0) did not equal (2000-01-01,Location(1.0,-1.0),10.0)
    [Lost Points] 5

  [Test Description] [#1 - Data extraction] stations with no location are ignored
    [Observed Error] Error while decoding: java.lang.RuntimeException: Null value appeared in non-nullable field:
    - field (class: "scala.Double", name: "lat")
  - root class: "observatory.Extraction.TempStation"
  If the schema is inferred from a Scala tuple/case class, or a Java bean, please try to use scala.Option[_] or other nullable types (e.g. java.lang.Integer instead of int/scala.Int).
  newInstance(class observatory.Extraction$TempStation)
  :- assertnotnull(input[0, int, true], - field (class: "scala.Int", name: "stn"), - root class: "observatory.Extraction.TempStation")
  :  +- input[0, int, true]
  :- assertnotnull(input[1, int, true], - field (class: "scala.Int", name: "wban"), - root class: "observatory.Extraction.TempStation")
  :  +- input[1, int, true]
  :- assertnotnull(input[2, int, true], - field (class: "scala.Int", name: "month"), - root class: "observatory.Extraction.TempStation")
  :  +- input[2, int, true]
  :- assertnotnull(input[3, int, true], - field (class: "scala.Int", name: "day"), - root class: "observatory.Extraction.TempStation")
  :  +- input[3, int, true]
  :- assertnotnull(input[4, double, true], - field (class: "scala.Double", name: "temp"), - root class: "observatory.Extraction.TempStation")
  :  +- input[4, double, true]
  :- assertnotnull(input[7, double, true], - field (class: "scala.Double", name: "lat"), - root class: "observatory.Extraction.TempStation")
  :  +- input[7, double, true]
  :- assertnotnull(input[8, double, true], - field (class: "scala.Double", name: "lon"), - root class: "observatory.Extraction.TempStation")
  :  +- input[8, double, true]
  :- assertnotnull(input[5, int, true], - field (class: "scala.Int", name: "s_stn"), - root class: "observatory.Extraction.TempStation")
  :  +- input[5, int, true]
  +- assertnotnull(input[6, int, true], - field (class: "scala.Int", name: "s_wban"), - root class: "observatory.Extraction.TempStation")
  +- input[6, int, true]

*/

}
