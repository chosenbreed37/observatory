package observatory

import java.time.LocalTime
/**
  * 4th milestone: value-added information
  */
object Manipulation2 {

  def generateMapping(temperatures: Iterable[(Location, Double)]): scala.collection.mutable.Map[Location, Double] = {
    val mapping = scala.collection.mutable.Map[Location, Double]()

    val points = for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180
    } yield (lat, lon)

    val items = points.par.map(p => {
      val (lat, lon) = p
      val loc = Location(lat, lon)
      val t = Visualization.predictTemperature(temperatures, loc)
      mapping += (loc -> t)
    })
    mapping
  }

  def getMapping(year: Int): scala.collection.mutable.Map[Location, Double] = {
    val filepath = s"target/grids/"
    val filename = filepath + s"${year}.csv"
    val file = new java.io.File(filename)
    if (file.exists) {
      val mapping = scala.collection.mutable.Map[Location, Double]()
      val stream = new java.io.FileInputStream(filename)
      val lines = scala.io.Source.fromInputStream(stream).getLines
      lines.foreach(line => {
        val values = line.split(",").map(_.trim)
        val lat = values(0).toDouble
        val lon = values(1).toDouble
        val t = values(2).toDouble
        mapping += (Location(lat, lon) -> t)
      })
      mapping
    } else {
      val path = new java.io.File(filepath)
      path.mkdirs()
      val temperatures = Extraction.locationYearlyAverageRecords3(Extraction.locationTemperatures(year))
      val mapping = generateMapping(temperatures)
      val writer = new java.io.FileWriter(filename)
      mapping.foreach(kv => {
        val loc = kv._1
        val t = kv._2
        writer.append(s"${loc.lat},${loc.lon},${t}")
        writer.append("\n")
      })
      writer.flush()
      writer.close()
      mapping
    }
  }

  def makeGrid(year: Int): (Int, Int) => Double = {
    println(s"${LocalTime.now}: makeGrid...generateMapping...")
    val mapping = getMapping(year)
    println(s"${LocalTime.now}: makeGrid...generateMapping...completed")
    (lat, lon) => {
      val loc = Location(lat, lon)
      mapping.getOrElse(loc, -37d)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    println(s"${LocalTime.now}: makeGrid...generateMapping...")
    val mapping = generateMapping(temperatures)
    println(s"${LocalTime.now}: makeGrid...generateMapping...completed")
    (lat, lon) => {
      val loc = Location(lat, lon)
      mapping.getOrElse(loc, -37d)
    }
  }

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val init = List[(Int, Int) => Double]()
    val grids = temperatures.foldLeft(init)((acc, curr) => {
      makeGrid(curr)::acc
    })
    (lat, lon) => {
      val (total, count) = grids.foldLeft(0d, 0)((acc, curr) => {
        val (runningTotal, runningCount) = acc
        val t = curr(lat, lon)
        (runningTotal + t, runningCount + 1)
      })
      total / count
    }
  }

  def average(start: Int, end: Int): (Int, Int) => Double = {
    val init = List[(Int, Int) => Double]()
    val grids = (start to end).foldLeft(init)((acc, curr) => {
      makeGrid(curr)::acc
    })
    (lat, lon) => {
      val (total, count) = grids.foldLeft(0d, 0)((acc, curr) => {
        val (runningTotal, runningCount) = acc
        val t = curr(lat, lon)
        (runningTotal + t, runningCount + 1)
      })
      total / count
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)],
    normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)
    (lat, lon) => {
      val c = grid(lat, lon)
      val r = normals(lat, lon)
      c - r
    }
  }

  def deviation(year: Int, normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(year)
    (lat, lon) => {
      val c = grid(lat, lon)
      val r = normals(lat, lon)
      c - r
    }
  }

}
