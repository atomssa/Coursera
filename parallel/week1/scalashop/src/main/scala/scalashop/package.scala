
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var xx: Int = x - radius
    var yy: Int = y - radius
    var totR: Int = 0
    var totG: Int = 0
    var totB: Int = 0
    var totA: Int = 0
    var count: Int = 0
    while (xx <= x + radius) {
      while (yy <= y + radius) {
        if ( (clamp(xx, 0, src.width - 1) == xx) && (clamp(yy, 0, src.height - 1) == yy)) {
          val srcRGBA = src(xx, yy)
          totR = totR + red(srcRGBA)
          totG = totG + green(srcRGBA)
          totB = totB + blue(srcRGBA)
          totA = totA + alpha(srcRGBA)
          count = count + 1
        }
        yy = yy + 1
      }
      xx = xx + 1
      yy = y - radius
    }
    rgba(totR/count, totG/count, totB/count, totA/count)
  }

}
