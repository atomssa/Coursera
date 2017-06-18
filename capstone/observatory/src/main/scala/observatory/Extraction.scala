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

  def vv = false
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

    if (vv) println("locateTemperaturesIn: Input")

    val tempDf: DataFrame = spark.read.schema(tempRecordSchema).option("header", value = false).csv(fsPath(temperaturesFile))
    val stationDf: DataFrame = spark.read.schema(stationSchema).option("header", value = false).csv(fsPath(stationsFile))

    if (vv) {
      tempDf.show(20)
      stationDf.show(10)
    }

    val tempDs = tempDf.as[TempRecord].where(
      tempDf("month").isNotNull and tempDf("day").isNotNull and tempDf("temp").isNotNull)
    val stationDs = stationDf.as[Station].where(
      stationDf("lat").isNotNull and stationDf("lon").isNotNull)

    if (vv) {
      println("@@@ stations dataset: ")
      stationDs.show(20)
      println("@@@ temperatures dataset: ")
      tempDs.show(20)
      println("@@@  joining stations and temperatures")
    }

    val tempStationDs = tempDs.join(stationDs,
      tempDs("stn") <=> stationDs("s_stn") and tempDs("wban") <=> stationDs("s_wban")
    ).as[TempStation]

    if (vv) {
      println("@@@ join result dataset: ")
      tempStationDs.show(20)
    }

    def convert(ts: TempStation): (LocalDate, Location, Double) = {
      val localDate = LocalDate.of(year, ts.month, ts.day)
      val location = Location(ts.lat, ts.lon)
      (localDate, location, (ts.temp - 32.0) * 5.0 / 9.0 )
    }

    val res = tempStationDs.collect.map(convert)
    if (vv) {
      println("locateTemperatures: Output ")
      res.take(10).foreach(println(_))
    }
    res
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    if (vv) {
      println("locationYearlyAverageRecords: Input")
      records.take(10).foreach(println(_))
    }
    val grpByLoc = records.groupBy( a => a._2)
    def avgTemp(it: Iterable[(LocalDate, Location, Double)]): Double = {
      val temps: Iterable[Double] = it.map(_._3)
      temps.sum / temps.size
    }
    val ans = grpByLoc.map( a => (a._1, avgTemp(a._2)))
    if (vv) {
      println("locationYearlyAverageRecords: Output")
      ans.take(10).foreach(println(_))
    }
    ans
  }

  final case class TempRecord(stn: Integer, wban: Integer, month: Int, day: Int, temp: Double)
  final case class Station(s_stn: Integer, s_wban: Integer, lat: Double, lon: Double)
  final case class TempStation(stn: Integer, wban: Integer, month: Int, day: Int, temp: Double, s_stn: Integer, s_wban: Integer, lat: Double, lon: Double)

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

}
