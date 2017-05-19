package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val z = Math.pow(2, zoom)
    val lat = Math.atan(Math.sinh(Math.PI - y/z*2*Math.PI))*180/Math.PI
    val lon = x/z*360 - 180
    Location(lat, lon)
  }

  def toArrayPosition(x: Int, y: Int): Int = {
    val c = 256
    x + y * c
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)],
    colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
      val n = 256
      val alpha = 127
      val pixels = new Array[Pixel](n * n)

      val points = for {
        x1 <- 0 until n
        y1 <- 0 until n
      } yield (x1, y1)
      val items = points.par.map(p => {
        val (x1, y1) = p
        val l = tileLocation(zoom + 8, x * n + x1, y * n + y1)
        val t = Visualization.predictTemperature(temperatures, l)
        val c = Visualization.interpolateColor(colors, t)
        (x1 + y1 * n, Pixel(c.red, c.green, c.blue, alpha))
      })
      items.foreach(kv => {
        val (i, p) = kv
        pixels(i) = p
      })
      Image(n, n, pixels)
  }

    type TemperatureData = (Iterable[(Location, Double)], Iterable[(Double, Color)])

  val colorScale = List(
    (60d, Color(255, 255, 255)),
    (32d, Color(255, 0, 0)),
    (12d, Color(255, 255, 0)),
    (0d, Color(0, 255, 255)),
    (-15d, Color(0, 0, 255)),
    (-27d, Color(255, 0, 255)),
    (-50d, Color(33, 0, 107)),
    (-60d, Color(0, 0, 0))
  )

  def getZoomLevels(max: Int): Iterable[(Int, Iterable[(Int, Int)])] =
    (0 until max).map(n =>
      (n, for {
        x <- 0 until Math.pow(2, n).toInt
        y <- 0 until Math.pow(2, n).toInt
      } yield (x, y))
    )

  def getTemperatures(year: Int): Iterable[(Location, Double)] = {
    val rawTemperatures = Extraction.locateTemperatures(year, "/stations.csv", s"/${year}.csv")
    Extraction.locationYearlyAverageRecords(rawTemperatures)
  }

  def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: TemperatureData): Unit = {
    val temperatures = data._1
    val colors = data._2

    val image = tile(temperatures, colors, zoom, x, y)
    val filename = s"target/temperatures/${year}/${zoom}/${x}-${y}.png"
    val file = new java.io.File(filename)
    image.output(file)
    println(s">>> ${java.time.LocalTime.now} | Tile: (${x}-${y})")
  }

  def generateImage2(year: Int, zoom: Int, x: Int, y: Int): Unit = {
    val image = Image(256, 256)
    val filename = s"target/temperatures/${year}/${zoom}/${x}-${y}.png"
    val file = new java.io.File(filename)
    image.output(file)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    val zoomLevels = getZoomLevels(4)
    yearlyData.toList.foreach(kv => {
      val (year, data) = kv
      zoomLevels.toList.foreach(kv1 => {
        val (zoom, tiles) = kv1
        val filepath = s"target/temperatures/${year}/${zoom}"
        val path = new java.io.File(filepath)
        path.mkdirs()
        tiles.toList.foreach(kv2 => {
          val (x, y) = kv2
          generateImage(year, zoom, x, y, data)
        })
      })
    })
  }

}
