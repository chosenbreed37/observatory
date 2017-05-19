package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("predict 1") {
    val scale = List((Location(0.0, 0.0), 10.0))
    val value = Location(90, -176.0)
    val expected = 10
    val actual = Visualization.predictTemperature(scale, value)
    assert(actual == expected)
  }

  test("predict 2") {
    val scale = List((Location(0.0, 0.0), 10.0))
    val value = Location(0.0, 0.0)
    val expected = 10
    val actual = Visualization.predictTemperature(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 1") {
    val scale = List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255)))
    val value = -0.5
    val expected = Color(128,0,128)
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 2") {
    val scale = List((0.0,Color(255,0,0)), (2321.0,Color(0,0,255)))
    val value = 1160.5
    val expected = Color(128,0,128)
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 3") {
    val scale = List((-1.6777223E7,Color(255,0,0)), (-1.0,Color(0,0,255)))
    val value = -8388612.0
    val expected = Color(128,0,128)
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 4") {
    val expected = Color(255,0,0)
    val scale = List((1.0,Color(255,0,0)), (52.70641388557618,Color(0,0,255)))
    val value = 1.0
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 5") {
    val expected = Color(255,0,0)
    val scale = List((-98.98227415865186,Color(255,0,0)), (-67.02447631752028,Color(0,0,255)))
    val value = -108.98227415865186
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("color interpolation 6") {
    val expected = Color(0, 128, 128)
    val scale = List((1.0,Color(255,0,0)), (2.0,Color(0,0,255)), (3.0,Color(0,255,0)))
    val value = 2.5
    val actual = Visualization.interpolateColor(scale, value)
    assert(actual == expected)
  }

  test("toArrayPosition 1") {
    val x = 0
    val y = 0
    val expected = 0
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 2") {
    val x = 1
    val y = 0
    val expected = 1
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 3") {
    val x = 359
    val y = 0
    val expected = 359
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 4") {
    val x = 0
    val y = 1
    val expected = 360
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 5") {
    val x = 1
    val y = 1
    val expected = 361
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 6") {
    val x = 1
    val y = 2
    val expected = 721
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("toArrayPosition 7") {
    val x = 359
    val y = 179
    val expected = 64799
    val actual = Visualization.toArrayPosition(x, y)
    assert(actual == expected)
  }

  test("visualize 1") {
    val temperatures = List((Location(45.0,-90.0),36.1643835403579), (Location(-45.0,0.0),-21.219052136494582))
    val colors = List((36.1643835403579,Color(255,0,0)), (-21.219052136494582,Color(0,0,255)))
    val image = Visualization.visualize(temperatures, colors)
    // image.output(new java.io.File("target/visualize1.png"))
  }
}
