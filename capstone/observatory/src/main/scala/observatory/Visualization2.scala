package observatory

import com.sksamuel.scrimage.Image
import observatory.Interaction.invWebMercatorRel
import observatory.Visualization.{interpolateColor, whArray}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val tileWidth: Int = Res.tw
  val tileHeight: Int = Res.th

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    val xu = d00 + x * (d10 - d00)
    val xl = d01 + x * (d11 - d01)
    xu + y * (xl - xu)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    import com.sksamuel.scrimage.{Color => sColor}
    val sortedColors = colors.toList.sortBy(_._1)
    val pixels = whArray(tileWidth, tileHeight).map{ case(xPixRel, yPixRel) =>
      val loc = invWebMercatorRel(zoom, x, y, xPixRel, yPixRel)
      val xx = loc.lon - Math.floor(loc.lon)
      val yy = Math.ceil(loc.lat) - loc.lat
      val fx = Math.floor(loc.lon).toInt
      val cy = Math.ceil(loc.lat).toInt
      val v = bilinearInterpolation(xx, yy, grid(cy, fx), grid(cy-1, fx), grid(cy, fx+1), grid(cy-1, fx+1))
//      if (xPixRel==0) println(s"visualizeGrid: Calculating color for: Z=$zoom, x=$x, y=$y, pixel: $xPixRel, $yPixRel")
      val col = interpolateColor(sortedColors, v)
      sColor(col.red, col.green, col.blue, 127).toPixel
    }
    Image(tileWidth, tileHeight, pixels.toArray)
  }

}
