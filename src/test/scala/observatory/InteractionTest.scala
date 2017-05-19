package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  test("generateImage 1975 zoom 0") {
    val year = 1975
    val zoom = 2
    val x = 1
    val y = 2
    val t0 = System.nanoTime()
    val temperatures = Interaction.getTemperatures(1975)
    val t1 = System.nanoTime()
    println(s"getTemperatures: ${(t1 - t0)} ns")
    val t2 = System.nanoTime()
    // Interaction.generateImage(year, zoom, x, y, (temperatures, Interaction.colorScale))
    val yearlyData = List((year, (temperatures, Interaction.colorScale)))
    Interaction.generateTiles[Interaction.TemperatureData](yearlyData, Interaction.generateImage)
    val t3 = System.nanoTime()
    println(s"generateImage: ${(t3 - t2)} ns")
  }

  test("getZoomLevels 0") {
    val expected = (0, Vector((0, 0)))
    val actual = Interaction.getZoomLevels(1).head
    assert(actual == expected)
  }

  test("getZoomLevels 1") {
    val expected = (1, Vector((0, 0), (0, 1), (1, 0), (1, 1)))
    val actual = Interaction.getZoomLevels(2).drop(1).head
    assert(actual == expected)
  }

  test("getZoomLevels 2") {
    val expected = 16
    val actual = Interaction.getZoomLevels(3).drop(2).head._2.size
    assert(actual == expected)
  }

  test("getZoomLevels 3") {
    val expected = 64
    val actual = Interaction.getZoomLevels(4).drop(3).head._2.size
    assert(actual == expected)
  }

}
