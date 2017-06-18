package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val vv = false

  def dist(l1: Location, l2: Location): Double = Math.hypot(l1.lat-l2.lat, l1.lon - l2.lon)
  def webMercator(z: Int, l: Location): (Int, Int) = {
    (
      (128 * Math.pow(2.0, z) * ( dtr(l.lon) + Math.PI ) / Math.PI).round.toInt,
      (128 * Math.pow(2.0, z) * ( Math.PI - Math.log( Math.tan( (Math.PI/4.0) + (dtr(l.lat)/2.0) ) ) ) / Math.PI).round.toInt
    )
  }

  def invWebMercatorAbs(z: Int, xPix: Int, yPix: Int): Location = {
    val lon = (Math.PI * xPix / 128.0 / Math.pow(2.0, z)) - Math.PI
    val lat = 2.0 * ( Math.atan( Math.pow( Math.E, Math.PI - (Math.PI * yPix / 128.0 / Math.pow(2.0, z)) ) ) - Math.PI/4.0)
    val boundLon = if (lon < -Math.PI) -Math.PI else if (lon > Math.PI) Math.PI else lon
    val boundLat = if (lat < -maxLatRad) -maxLatRad else if (lat > maxLatRad) maxLatRad else lat
    Location(rtd(boundLat), rtd(boundLon))
  }

  val maxLat = 85.0511
  val maxLon = 180.0
  val maxLatRad = dtr(maxLat)

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    assert(x >=0 && x < Math.pow(2, zoom))
    assert(y >=0 && y < Math.pow(2, zoom))
    val res = invWebMercatorAbs(zoom, x * 256, y * 256)
    //println(s"tileLocation: zoom=$zoom x=$x, y=$y => res=$res")
    res
  }

  def invWebMercatorRel(z: Int, x: Int, y: Int, xPixRel: Int, yPixRel: Int): Location =
    invWebMercatorAbs(z, x * 256 + xPixRel, y * 256 + yPixRel)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {

    val sortedColors = colors.toList.sortBy(_._1)

    if (vv) println("visualize arguments: ")
    val temps = temperatures.map(a=>s"[(${a._1.lat},${a._1.lon}),T=${a._2}]").mkString(", ")
    if (vv) println(s"temps: $temps")
    if (vv) println(s"colors: $colors")
    if (vv) println(s"z=$zoom, x=$x, y=$y")

    import com.sksamuel.scrimage.{Color => sColor}
    val pixels = whArray(256, 256).map{ case(xPixRel, yPixRel) =>
      val loc = invWebMercatorRel(zoom, x, y, xPixRel, yPixRel)
      val col = interpolateColor(sortedColors, predictTemperature(temperatures, loc))
      sColor(col.red, col.green, col.blue).toPixel
    }
    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {

    for {z <- 0 to 3
         x <- 0 until Math.pow(2,z).toInt
         y <- 0 until Math.pow(2,z).toInt
         data <- yearlyData
    } {
//      println(s"generating tiles for z=$z, x=$x, y=$y, d=${data._1}")
      generateImage(data._1, z, x, y, data._2)
    }

  }

}
