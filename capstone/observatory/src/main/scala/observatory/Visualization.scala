package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  def dtr(d: Double): Double = d * Math.PI/180.0
  val earthRadius: Double = 6371.0
  val powerPar: Double = 2.0
  def dist(l1: Location, l2: Location): Double =
    earthRadius * Math.abs(
      Math.acos(
        Math.sin(dtr(l1.lat)) * Math.sin(dtr(l2.lat)) +
          Math.cos(dtr(l1.lat)) * Math.cos(dtr(l2.lat)) * Math.cos(dtr(l1.lon) - dtr(l2.lon))))
  def weight(l1: Location, l2: Location): Double = 1/Math.pow(dist(l1,l2),powerPar)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
//    val nearest = temperatures.filter{case(l,_)=>dist(l,location)<1.0}.toList
    val nearest = temperatures.minBy(a => dist(a._1,location))
    if (dist(nearest._1, location) < 1.0) {
      nearest._2
    } else {
      val sumWeights = temperatures.map { case (l, t) =>
        val w = weight(l, location)
        (w * t, w)
      }.foldLeft((0.0, 0.0))((lhs, rhs) => (lhs._1 + rhs._1, lhs._2 + rhs._2))
      if (sumWeights._2 != 0) sumWeights._1 / sumWeights._2
      else 9999.0
    }
  }

  def avgComp(r: Double, c1: Int, c2: Int): Int = c1 + ((c2 - c1)*r).toInt
  def interp(r: Double, c1: Color, c2: Color): Color =
    Color(avgComp(r, c1.red, c2.red), avgComp(r, c1.red, c2.red), avgComp(r, c1.blue, c2.blue))
  def avgColor(v: Double, c1: (Double,Color), c2: (Double,Color)): Color = {
    if (c1._1 < c2._1) interp( (v-c1._1)/(c2._1-c1._1), c1._2, c2._2)
    else interp( (v-c2._1)/(c1._1-c2._1), c2._2, c1._2)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val nearest = points.toList.sortBy(a => Math.abs(a._1 - value)).take(2)
    assert(nearest.size >= 2, "Number of interpolation colors < 2")
    avgColor(value, nearest(0), nearest(0))
  }

  val alpha = 255
  val width = 360
  val height = 180
  def pixelToLoc(iw: Int, ih: Int): Location = Location(360*iw/width - (width/2), 180*ih/height - (height/2))
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

//    val pixels: Array[Pixel] = Array.fill[Pixel](width*height)(Pixel(0,0,0,0))
    val pixels2 =
      for {iw <- (0 until width).toArray
           ih <- (0 until height).toArray
      } yield {
        val col = interpolateColor(colors, predictTemperature(temperatures, pixelToLoc(iw, ih)))
        Pixel(col.red, col.green, col.blue, alpha)
      }
    val image = Image(width, height, pixels2)

    image.output(new java.io.File("/Users/tujuba/Desktop/youpi.png"))

    image

  }

}

