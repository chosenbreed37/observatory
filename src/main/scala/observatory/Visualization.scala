package observatory

import java.awt.image.BufferedImage
import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val earthRadius = 6371.0

  def sin(x: Double) =
    Math.sin(Math.toRadians(x))

  def cos(x: Double) =
    Math.cos(Math.toRadians(x))

  def distance(x: Location, xi: Location): Double = {
    earthRadius *
      Math.acos(sin(x.lat) * sin(xi.lat)
        + cos(x.lat) * cos(xi.lat) * cos(xi.lon - x.lon))
  }

  def weighting(d: Double, p: Double): Double = {
    1.0 / Math.pow(d, p)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val p = 5

    val (num, den) = temperatures.foldLeft((0.0, 0.0))((acc, curr) => {
      val d = distance(location, curr._1)
      val w = weighting(d, p)
      val (num, den) = acc
      if (d <= 1.0) (curr._2, 1)
      else (num + curr._2 * w, den + w)
    })

    num / den
  }

  def interpolate(x: Double, p1: (Double, Color), p2: (Double, Color)): Color = {
    def interpolateC(x0: Double, y0: Int, x1: Double, y1: Int): Int = {
      val num = BigDecimal(y0) * BigDecimal(x1 - x) + BigDecimal(y1) * BigDecimal(x - x0)
      val den = BigDecimal(x1 - x0)
      Math.round((num / den).toFloat)
    }
    val b = interpolateC(p1._1, p1._2.blue, p2._1, p2._2.blue)
    val r = interpolateC(p1._1, p1._2.red, p2._1, p2._2.red)
    val g = interpolateC(p1._1, p1._2.green, p2._1, p2._2.green)
    Color(r, g, b)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    val points1 = points.toList
    val max = points1.maxBy(p => p._1)
    val min = points1.minBy(p => p._1)
    val p1 = points1.sortWith(_._1 > _._1).find(x => x._1 <= value)
    val p2 = points1.sortWith(_._1 < _._1).find(x => x._1 > value)
    val c = (p1, p2) match {
      case (Some(r1), Some(r2)) => interpolate(value, r1, r2)
      case (None, Some(r2)) => min._2
      case (Some(r1), None) => max._2
      case _ => min._2
    }
    c
  }

  def toArrayPosition(x: Int, y: Int): Int = {
    val c = 360
    x + y * c
  }

  def tileLocation(x: Int, y: Int): Location = {
    Location(x - 180, 90 - y)
  }

  def toPixel(c: Color) = Pixel(c.red, c.green, c.blue, 0)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val w = 360
    val h = 180
    val alpha = 255
    val pixels = new Array[Pixel](w * h)

    for {
      x <- 0 until w
      y <- 0 until h
    } yield {
      val l = Location(90 - y, x - 180)
      val t = predictTemperature(temperatures, l)
      val c = interpolateColor(colors, t)
      pixels(x + y * w) = Pixel(c.red, c.green, c.blue, alpha)
    }
    Image(w, h, pixels)
  }
}
