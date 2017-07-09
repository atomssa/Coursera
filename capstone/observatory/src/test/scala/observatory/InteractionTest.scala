package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("Check invMercador(mercador(location))=location ") {
    val loc = Location(45,10)
    val z = 1
    val merc = Interaction.webMercator(z, loc)
    println(s"merc = (${merc._1}, ${merc._2})")
    val loc2 = Interaction.invWebMercatorAbs(z, merc._1, merc._2)
    val dist = Interaction.dist(loc,loc2)
    println(s"dist roundtrip = $dist")
    assert(dist < 1)
  }

  test("test invMercadorAbs, z=1, origin") {
    val loc = Interaction.invWebMercatorAbs(1, Res.tw, Res.th)
    println(s"invMercAbs(1,${Res.tw},${Res.th})=$loc")
    val dist = Interaction.dist(loc, Location(0,0))
    assert(dist < 0.1)
  }

  test("test invMercatorRel, bottom right z=1,x=0,y=0") {
    val loc = Interaction.invWebMercatorRel(1,0,0,Res.tw,Res.th)
    println(s"invMercRel(1,0,0,${Res.tw},${Res.th})=$loc")
    val dist = Interaction.dist(loc, Location(0,0))
    assert(dist < 0.1)
  }

  test("test invMercatorRel, top left z=1,x=1,y=1") {
    val loc = Interaction.invWebMercatorRel(1,1,1,0,0)
    println(s"invMercRel(1,1,1,0,0)=$loc")
    val dist = Interaction.dist(loc, Location(0,0))
    assert(dist < 0.1)
  }

  test("test invMercatorRel, bottom right z=1,x=1,y=1") {
    val loc = Interaction.invWebMercatorRel(1,1,1,Res.tw,Res.th)
    println(s"invMercRel(1,0,0,${Res.tw},${Res.th})=$loc")
    val dist = Interaction.dist(loc, Location(-Interaction.maxLat, Interaction.maxLon))
    assert(dist < 0.1)
  }

  test("test generate tiles funciton") {
    //val data = Iterable((2000,0.0),(2001,1.0),(2002,2.0),(2003,3.0),(2004,4.0))
    val data = Iterable((2000,0.0))
    def func (z: Int, x: Int, y: Int, year: Int, d: Double) = {
      println(s"generating tiles for z=$z, x=$x, y=$y, year=$year, d=$d")
    }
    Interaction.generateTiles[Double](data, func)
    assert(1===1)
  }

}
