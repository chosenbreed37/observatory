package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import java.time.LocalDate
import java.time.LocalTime

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

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

  def generateImage(grid: (Int, Int) => Double, year: Int, zoom: Int, x: Int, y: Int): Unit = {
    val image = visualizeGrid(grid, colorScale, zoom, x, y)
    val filepath = s"target/deviations/${year}/${zoom}"
    val path = new java.io.File(filepath)
    path.mkdirs()
    val filename = s"target/deviations/${year}/${zoom}/${x}-${y}.png"
    val file = new java.io.File(filename)
    image.output(file)
  }

  def generateImageFake(grid: (Int, Int) => Double, year: Int, zoom: Int, x: Int, y: Int): Unit = {
    println(s"${LocalTime.now}: generateImageFake...")
    println(s"${LocalTime.now}: visualize grid fake...")
    val image = visualizeGridFake(grid, colorScale, zoom, x, y)
    val filepath = s"target/deviations/${year}/${zoom}"
    val path = new java.io.File(filepath)
    path.mkdirs()
    val filename = s"target/deviations/${year}/${zoom}/${x}-${y}.png"
    val file = new java.io.File(filename)
    image.output(file)
  }

  def generateTiles(start: Int, end: Int,
    generateImage: ((Int, Int) => Double, Int, Int, Int, Int) => Unit
  ): Unit = {
    val zoomLevels = Interaction.getZoomLevels(4)
    (start to end).foreach(year => {
      println(s"generating tiles for ${year}...")
      val grid = Manipulation2.deviation(year, normals)
      zoomLevels.toList.foreach(kv1 => {
        val (zoom, tiles) = kv1
        tiles.toList.foreach(kv2 => {
          val (x, y) = kv2
          generateImage(grid, year, zoom, x, y)
        })
      })
      println(s"generating tiles for ${year}...completed")
    })
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

  def visualizeGridFake(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val n = 128
    val alpha = 127
    val pixels = new Array[Pixel](n * n)

    println(s"${LocalTime.now}: generate points...")

    val points = for {
      x1 <- 0 until n
      y1 <- 0 until n
    } yield (x1, y1)

    val items = points.par.map(p => {
      val (x1, y1) = p
      val l = Interaction.tileLocation(zoom + 7, x * n + x1, y * n + y1)
      val usp = unitSquarePoints(l.lat, l.lon)
      val us = unitSquare(usp, grid)
      val t = bilinearInterpolation(l.lon - usp.d00._2, usp.d00._1 - l.lat, us.d00, us.d01, us.d10, us.d11)
      val c = Visualization.interpolateColor(colors, t)
      (x1 + y1 * n, Pixel(c.red, c.green, c.blue, alpha))
    })
    println(s"${LocalTime.now}: construct the image...")
    items.foreach(kv => {
      val (i, p) = kv
      pixels(i) = p
    })
    Image(n, n, pixels).scaleTo(256, 256)
  }

  def computeYearlyAverages(start: Int, end: Int): Iterable[Iterable[(Location, Double)]] = {
    println(s"${LocalTime.now}: computeYearlyAverages...")
    val init = List[Iterable[(Location, Double)]]()
    val result = (start to end).foldLeft(init)((acc, year) => {
      println(s"${LocalTime.now}: Processing ${year}...")
      val avgs = Extraction.locationYearlyAverageRecords3(Extraction.locationTemperatures(year))
      println(s"${LocalTime.now}: Processing ${year}...completed")
      avgs::acc
    })
    println(s"${LocalTime.now}: computeYearlyAverages...completed")
    result
  }

  def computeYearlyAverages2(start: Int, end: Int): Map[Int, Iterable[(Location, Double)]] = {
    println(s"${LocalTime.now}: computeYearlyAverages2...")
    val init = Map[Int, Iterable[(Location, Double)]]()
    val result = (start to end).foldLeft(init)((acc, year) => {
      println(s"${LocalTime.now}: Processing ${year}...")
      val avgs = Extraction.locationYearlyAverageRecords3(Extraction.locationTemperatures(year))
      println(s"${LocalTime.now}: Processing ${year}...completed")
      acc + (year -> avgs)
    })
    println(s"${LocalTime.now}: computeYearlyAverages2...completed")
    result
  }

  lazy val normals: (Int, Int) => Double = {
    val result = Manipulation2.average(1975, 1989)
    result
  }

  // lazy val deviations: (Int) => (Int, Int) => Double = {
  //   val avgs = computeYearlyAverages2(1990, 2015)
  //   (year) => {
  //     avgs.get(year) match {
  //       case Some(temperatures) => Manipulation2.deviation(temperatures, normals)
  //       case _ => normals
  //     }
  //   }
  // }

  def computeDeviationsByYear(start: Int, end: Int): Map[Int, (Int, Int) => Double] = {
    println(s"${LocalTime.now}: computing deviations...")
    val result = computeYearlyAverages2(start, end)
      .map(kv => {
        val (year, temperatures) = kv
        (year -> Manipulation2.deviation(temperatures, normals))
      })
    println(s"${LocalTime.now}: computing deviations...completed")
    result
  }

  lazy val deviationsByYear = computeDeviationsByYear(1990, 2015)
}
