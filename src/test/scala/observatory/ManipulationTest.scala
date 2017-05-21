package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {
  test("makeGrid 1") {
    val temperatures = List((Location(0.0, 0.0), 10.0))
    val (lat, lon) = (90, -176)
    val expected = 10
    val actual = Manipulation.makeGrid(temperatures)(lat, lon)
    assert(actual == expected)
  }

  test("makeGrid 2") {
    val temperatures = List((Location(0.0, 0.0), 10.0))
    val (lat, lon) = (0, 0)
    val expected = 10
    val actual = Manipulation.makeGrid(temperatures)(lat, lon)
    assert(actual == expected)
  }

  test("average 1") {
    val temperatures = List(List((Location(0.0, 0.0), 10.0)),
      List((Location(0.0, 0.0), 10.0)))
    val (lat, lon) = (0, 0)
    val expected = 10
    val actual = Manipulation.average(temperatures)(lat, lon)
    assert(actual == expected)
  }
}
