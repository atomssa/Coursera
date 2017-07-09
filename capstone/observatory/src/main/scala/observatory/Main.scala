package observatory

import java.io.File
import java.util.Calendar

object Main extends App {

  def println(x: Any): Unit = {
    Predef.println( curTime + ": " + x)
  }

  def curTime: String = {
    val now = Calendar.getInstance()
    val hh = now.get(Calendar.HOUR)
    val mm = now.get(Calendar.MINUTE)
    val ss = now.get(Calendar.SECOND)
    val ms = now.get(Calendar.MILLISECOND)
    f"$hh%02d:$mm%02d:$ss%02d.$ms%03d"
  }

  val colorScaleTemp = Iterable(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))

  val colorScaleDev = Iterable(
    (7.0,  Color(0,   0,   0)),
    (4.0,  Color(255, 0,   0)),
    (2.0,  Color(255, 255, 0)),
    (0.0,  Color(255, 255, 255)),
    (-2.0, Color(0,   255, 255)),
    (-7.0, Color(0,   0,   255)))

    val years = 1975 to 2015
//  val years = 1975 to 1976

  val targTempDir = "/Users/tujuba/IdeaProjects/Coursera/capstone/observatory/target/temperatures/"
  val targTempFromGridDir = "/Users/tujuba/IdeaProjects/Coursera/capstone/observatory/target/temperaturesFromGrid/"
  val targDevDir = "/Users/tujuba/IdeaProjects/Coursera/capstone/observatory/target/deviations/"

  def avgTemp(year: Int): Iterable[(Location,Double)] = {
    println(s"reading data from resources file for year $year")
    Extraction.locationYearlyAverageRecords(
      Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv"))
  }

  def getFileHandle(path: String): File = {
    val f = new java.io.File(path)
    f.getParentFile.mkdirs
    f
  }

  val yearlyAverages = years.map(y => (y, avgTemp(y)))

//  Interaction.generateTiles[Iterable[(Location,Double)]](yearlyAverages,
//    (year: Int, zoom: Int, x: Int, y: Int, d: Iterable[(Location,Double)]) => {
//      val fName = s"/$year/$zoom/$x-$y.png"
//      println(s"generateTiles: z=$zoom, x=$x, y=$y, year=$year to file: $fName")
//      println(s"generating temperature tiles")
//      val tempImg = Interaction.tile(d, colorScaleTemp, zoom, x, y)
//      println(s"Saving temperature from grid tile")
//      tempImg.output(getFileHandle(targTempDir + fName))
//      println("---------------------------------------------------------")
//    })

  println("=========================================================")
  println("Making reference temperature grid for deviations")

  //val refYears = Iterable(1975, 1976, 1977, 1978, 1979)
  //val refTemps = yearlyAverages.filter(_._1 < 1980).map(_._2)
  //val refGrid = Manipulation.average(refTemps)

  val yearlyGrids = yearlyAverages.map{ya =>
    println(s"generating grid for ${ya._1}")
    (ya._1, Manipulation.makeGrid(ya._2))
  }
  val refGrid = Manipulation.averageGrids(yearlyGrids.filter(_._1 < 1980).map(_._2))

  println("Done generating grids")
  println("==========================================================")

  Interaction.generateTiles[(Int, Int)=>Double](yearlyGrids,
    (year: Int, zoom: Int, x: Int, y: Int, grid: (Int, Int)=> Double) => {
      val fName = s"/$year/$zoom/$x-$y.png"
      println(s"generateTiles (grid method): z=$zoom, x=$x, y=$y, year=$year to file: $fName")

      println(s"generating temperature tiles from grid")
      val tempImg2 = Visualization2.visualizeGrid(grid, colorScaleTemp, zoom, x, y)
      println(s"Saving temperature from grid tile")
//      tempImg2.output(getFileHandle(targTempFromGridDir + fName))
      tempImg2.output(getFileHandle(targTempDir + fName))
      println("Done with temprature part")

      if (year >= 1980) {
        println("generating Deviation tiles from grid and refGrid")
        val diffGrid = Manipulation.deviationGrid(grid, refGrid)
        val devImg = Visualization2.visualizeGrid(diffGrid, colorScaleDev, zoom, x, y)
        println(s"Saving deviation tile")
        devImg.output(getFileHandle(targDevDir + fName))
      }
      println("Done with deviation part")
      println("---------------------------------------------------------")

    }
  )

  Extraction.spark.close()

}
