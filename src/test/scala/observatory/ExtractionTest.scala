package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Ignore

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("converts from farenheit to celcius") {
    val expected = 37.77777777777778
    val actual = Extraction.farenheitToCelcius(100)

    assert(actual == expected)
  }

  test("can read stations") {
    var stations = Extraction.getStations("/stations.csv")
    val expected = Location(44.6, 175.2)
    val actual = stations(Station(997615, 0))
    assert(actual == expected)
  }
}
