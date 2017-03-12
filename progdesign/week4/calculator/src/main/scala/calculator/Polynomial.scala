package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal{
    val aa = a()
    val bb = b()
    val cc = c()
    (bb*bb) - (4.0*aa*cc)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal{
    val bb = b()
    val aa = a()
    val dd = delta()
    //val delta = computeDelta(a, b, c)()
    if (dd < 0) Set()
    else if (dd == 0) Set(-bb / aa / 2.0)
    else Set( ( -bb + Math.sqrt(dd)) / aa / 2.0, ( - bb - Math.sqrt(dd) )/ aa / 2.0 )
  }
}
