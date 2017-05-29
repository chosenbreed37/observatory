package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class Interaction2Test extends FunSuite with Checkers {

  test("available layers 1") {
    val expected =
      Layer(
        LayerName.Temperatures,
        Interaction.colorScale,
        1990 to 2015
      )
    val actual = Interaction2.availableLayers.head
    assert(actual == expected)
  }

  test("year bounds 1") {
    val temperatures =
      Layer(
        LayerName.Temperatures,
        Interaction.colorScale,
        1990 to 2015
      )
    val expected = Signal(1990 to 2015)
    val actual = Interaction2.yearBounds(Signal(temperatures))
    assert(actual() == expected())
  }

  test("yearSelection 1") {
    val selectedLayer = Signal(Interaction2.availableLayers.head)
    val sliderValue = Signal(1989)
    val expected = Signal(1990)
    val actual = Interaction2.yearSelection(selectedLayer, sliderValue)
    assert(actual() == expected())
  }

  test("yearSelection 2") {
    val selectedLayer = Signal(Interaction2.availableLayers.head)
    val sliderValue = Signal(2016)
    val expected = Signal(2015)
    val actual = Interaction2.yearSelection(selectedLayer, sliderValue)
    assert(actual() == expected())
  }

  test("yearSelection 3") {
    val selectedLayer = Signal(Interaction2.availableLayers.head)
    val sliderValue = Signal(1991)
    val expected = Signal(1991)
    val actual = Interaction2.yearSelection(selectedLayer, sliderValue)
    assert(actual() == expected())
  }

  test("layerUrlPattern 1") {
    val selectedLayer = Signal(Interaction2.availableLayers.head)
    val selectedYear = Signal(1991)
    val expected = Signal("target/temperatures/1991")
    val actual = Interaction2.layerUrlPattern(selectedLayer, selectedYear)
    assert(actual() == expected())
  }

  test("caption test") {
    val selectedLayer = Signal(Interaction2.availableLayers.head)
    val selectedYear = Signal(1991)
    val expected = Signal("Temperatures (1991)")
    val actual = Interaction2.caption(selectedLayer, selectedYear)
    assert(actual() == expected())
  }
}
