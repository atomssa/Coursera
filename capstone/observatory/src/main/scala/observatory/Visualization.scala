package observatory

import com.sksamuel.scrimage.Image

import scala.collection.parallel.mutable.ParArray

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val vv = false

  def dtr(d: Double): Double = d * Math.PI/180.0
  def rtd(r: Double): Double = r * 180.0 / Math.PI

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
    if (vv) println(s"Predicting temp for $location")
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

  def avgComp(r: Double, c1: Int, c2: Int): Int = c1 + ((c2 - c1)*r).round.toInt
  def interpolateColor(r: Double, c1: Color, c2: Color): Color =
    Color(avgComp(r, c1.red, c2.red), avgComp(r, c1.green, c2.green), avgComp(r, c1.blue, c2.blue))
  // Assumes (value of c1) <  v < (value of c2)
  def interpolateColor(v: Double, c1: (Double,Color), c2: (Double,Color)): Color = {
     interpolateColor( (v-c1._1)/(c2._1-c1._1), c1._2, c2._2)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    if (vv) println(s"interpolating color for value $value")
    // assumes scale is sorted
    def findBounds(v: Double, scale: List[(Double,Color)]): (Option[(Double,Color)],Option[(Double,Color)]) = {
      scale match {
        case Nil => (None,None)
        case p :: Nil => (None,Some(p))
        case p :: ps =>
          if ( v <= p._1) (Some(p), None)
          else if ( p._1 < v && v <= ps.head._1) (Some(p),Some(ps.head))
          else findBounds(v, ps)
      }
    }
    val sorted = points.toList.sortBy(_._1)
    if (vv) println(s"v=$value, sorted=$sorted")
    findBounds(value, sorted) match {
      case (None,None) => if (vv) println("nn"); throw new Exception("Empty color scale list provided")
      case (Some(l),None) => if (vv) println("Sn"); l._2
      case (None,Some(h)) => if (vv) println("nS"); h._2
      case (Some(l),Some(h)) => if (vv) println(s"S($l)S($h)"); interpolateColor(value, l, h)
    }
  }

  def pixelToLoc(iw: Int, ih: Int, width: Int, height: Int): Location = Location(90 - 180*ih/height, 360*iw/width - 180)
  def whArray(ww: Int, hh: Int): ParArray[(Int,Int)] =
    for {ih <- (0 until hh).toParArray
         iw <- (0 until ww).toParArray
    } yield { (iw,ih) }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val sortedColors = colors.toList.sortBy(_._1)

    if (vv) println("visualize arguments: ")
    val temps = temperatures.map(a=>s"[(${a._1.lat},${a._1.lon}),T=${a._2}]").mkString(", ")
    if (vv) println(s"temps: $temps")
    if (vv) println(s"colors: $colors")
    val col = interpolateColor(sortedColors, predictTemperature(temperatures, Location(90.0,-180.0)))
    if (vv) println(s"interpCol(90,-180) = $col")

    val width = 360
    val height = 180

    import com.sksamuel.scrimage.{Color => sColor}
    val pixels = whArray(width, height).map{ case(iw, ih) =>
      val col = interpolateColor(sortedColors, predictTemperature(temperatures, pixelToLoc(iw, ih, width, height)))
      sColor(col.red, col.green, col.blue, 127).toPixel
    }
    Image(width, height, pixels.toArray)
  }

}

