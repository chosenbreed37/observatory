package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Visualization2Test extends FunSuite with Checkers {

  test("unitSquare 1") {
    val lon = 112.3
    val lat = 45.7
    val expected = UnitSquarePoints((46, 112), (45, 112), (46, 113), (45, 113))
    val actual = Visualization2.unitSquarePoints(lat, lon)
    assert(actual == expected)
  }
}
