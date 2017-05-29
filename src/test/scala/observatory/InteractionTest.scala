package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {


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
