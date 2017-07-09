package observatory

import observatory.Visualization._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  def t(lat: Int, lon: Int): Int = (lon + 180) + (360 * (lat + 89) )
  def tiLon(i: Int): Int = (i % 360) - 180
  def tiLat(i: Int): Int = (i / 360) - 89

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val grid = (for (i <- (0 to 64800).toParArray) yield {
//      if (tiLon(i) == 0) println(s"makeGrid: grid point: ${tiLat(i)}, ${tiLon(i)}")
      predictTemperature(temperatures, Location(tiLat(i), tiLon(i)))
    }).toArray
    (lat: Int, lon: Int) => grid(t(lat, lon))
  }

//  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
//    (lat: Int, lon: Int) => {
//      predictTemperature(temperatures, Location(lat, lon))
//    }
//  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val grids = temperaturess.map(makeGrid)
    averageGrids(grids)
//    val cnt = grids.size
//    assert(cnt != 0)
//    val avg = (for (i <- (0 to 64800).toParArray) yield {
//      val lat: Int = tiLat(i); val lon: Int = tiLon(i)
//      grids.foldLeft(0.0)((agg, grid)=>agg + grid(lat,lon))/cnt
//    }).toArray
//    (lat: Int, lon: Int) => avg(t(lat, lon))
  }

  def averageGrids(grids: Iterable[(Int,Int)=>Double]): (Int, Int) => Double = {
    val cnt = grids.size
    assert(cnt != 0)
    val avg = (for (i <- (0 to 64800).toParArray) yield {
      val lat: Int = tiLat(i); val lon: Int = tiLon(i)
      grids.foldLeft(0.0)((agg, grid)=>agg + grid(lat,lon))/cnt
    }).toArray
    (lat: Int, lon: Int) => avg(t(lat, lon))
  }

  //  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
//    (lat: Int, lon: Int) => {
//      val agg = temperaturess.foldLeft((0.0,0))((agg,temps)=>(agg._1+makeGrid(temps)(lat,lon), agg._2+1))
//      if (agg._2 != 0) agg._1/agg._2 else throw new Exception("Empty temperatures")
//    }
//  }


  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val cur = makeGrid(temperatures)
    deviationGrid(cur, normals)
//    val dev = (for (i <- (0 to 64800).toParArray) yield {
//      val lat: Int = tiLat(i); val lon: Int = tiLon(i)
//      cur(lat, lon) - normals(lat, lon)
//    }).toArray
//    (lat: Int, lon: Int) => dev(t(lat, lon))
  }

  def deviationGrid(cur: (Int, Int) => Double, normals: (Int, Int) => Double): (Int, Int) => Double = {
    val dev = (for (i <- (0 to 64800).toParArray) yield {
      val lat: Int = tiLat(i); val lon: Int = tiLon(i)
      cur(lat, lon) - normals(lat, lon)
    }).toArray
    (lat: Int, lon: Int) => dev(t(lat, lon))
  }

//  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
//    (lat: Int, lon: Int) => { makeGrid(temperatures)(lat, lon) - normals(lat, lon) }
//  }

}

