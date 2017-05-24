package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

import java.time.LocalDate

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Observatory")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  val colorScale: Iterable[(Double, Color)] =
    List(
      (7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255))
    )


  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) +
    d10 * x * (1- y) +
    d01 * (1 - x) * y +
    d11 * x * y
  }

  def unitSquarePoints(lat: Double, lon: Double): UnitSquarePoints = {
    val lat1 = Math.ceil(lat).toInt
    val lat2 = Math.floor(lat).toInt
    val lon1 = Math.floor(lon).toInt
    val lon2 = Math.ceil(lon).toInt
    UnitSquarePoints((lat1, lon1), (lat2, lon1), (lat1, lon2), (lat2, lon2))
  }

  def unitSquare(points: UnitSquarePoints, grid: (Int, Int) => Double): UnitSquare = {
    UnitSquare(
      grid(points.d00._1, points.d00._2),
      grid(points.d01._1, points.d01._2),
      grid(points.d10._1, points.d10._2),
      grid(points.d11._1, points.d11._2))
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val n = 256
    val alpha = 127
    val pixels = new Array[Pixel](n * n)

    val points = for {
      x1 <- 0 until n
      y1 <- 0 until n
    } yield (x1, y1)

    val items = points.par.map(p => {
      val (x1, y1) = p
      val l = Interaction.tileLocation(zoom + 8, x * n + x1, y * n + y1)
      val usp = unitSquarePoints(l.lat, l.lon)
      val us = unitSquare(usp, grid)
      val t = bilinearInterpolation(l.lon - usp.d00._2, usp.d00._1 - l.lat, us.d00, us.d01, us.d10, us.d11)
      val c = Visualization.interpolateColor(colors, t)
      (x1 + y1 * n, Pixel(c.red, c.green, c.blue, alpha))
    })
    items.foreach(kv => {
      val (i, p) = kv
      pixels(i) = p
    })
    Image(n, n, pixels)
  }

  lazy val normals = {
    Extraction.locationYearlyAverageRecords((1975 to 1975)
      .flatMap(year =>
        Extraction.locateTemperatures(year, "/stations.csv", s"/${year}.csv"))
    )
  }

  lazy val averages = Extraction.locationAverages(Extraction.locationYearlyAverages("/yearly-averages.csv"))

  def computeNormals(start: Int, end: Int): Unit = {
    val writer = new java.io.FileWriter("normals.csv")
    println(s"${java.time.LocalTime.now}: computeNormals...")
    Extraction.locationYearlyAverageRecords3((start to end)
    .flatMap(year =>
      Extraction.locationTemperatures(year)
    ))
    .map(v => s"${v._1.lat},${v._1.lon},${v._2}")
    .foreach(l => {
      writer.append(l)
      writer.append("\n")
    })
    writer.flush()
    writer.close()
    println(s"${java.time.LocalTime.now}: computeNormals...completed")
  }

  def computeNormals2(start: Int, end: Int) = {
    println(s"${java.time.LocalTime.now}: computeNormals2...")
    val writer = new java.io.FileWriter("normals.csv", true)
    (start to end).foreach(year => {
      println(s"${java.time.LocalTime.now}: Processing ${year}...")
      Extraction.locationYearlyAverageRecords3((year to year)
        .flatMap(year =>
          Extraction.locationTemperatures(year)
      ))
      .map(v => s"${v._1.lat},${v._1.lon},${v._2}")
      .foreach(l => {
        writer.append(l)
        writer.append("\n")
      })
      println(s"${java.time.LocalTime.now}: Processing ${year}...completed")
    })
    writer.flush()
    writer.close()
    println(s"${java.time.LocalTime.now}: computeNormals2...completed")
  }

  def computeYearlyAverages(start: Int, end: Int): Iterable[Iterable[(Location, Double)]] = {
    println(s"${java.time.LocalTime.now}: computeYearlyAverages...")
    val init = List[Iterable[(Location, Double)]]()
    val result = (start to end).foldLeft(init)((acc, year) => {
      println(s"${java.time.LocalTime.now}: Processing ${year}...")
      val avgs = Extraction.locationYearlyAverageRecords3(Extraction.locationTemperatures(year))
      println(s"${java.time.LocalTime.now}: Processing ${year}...completed")
      avgs::acc
    })
    println(s"${java.time.LocalTime.now}: computeYearlyAverages...completed")
    result
  }
}
