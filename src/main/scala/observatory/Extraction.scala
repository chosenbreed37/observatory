package observatory

import java.time.LocalDate
import java.lang.{Class, ClassLoader}
import fs2.{io, text, Task}
import scala._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  def parseStation(line: String): Option[(Station, Location)] = {
    val values = line.split(",").map(_.trim)
    if (values.length == 4) {
      val stn = if (values(0) != "") values(0).toInt else 0
      val wban = if (values(1) != "") values(1).toInt else 0
      val latOp = if (values(2) != "") Some(values(2).toDouble) else None
      val lonOp = if (values(3) != "") Some(values(3).toDouble) else None
      (latOp, lonOp) match {
        case (Some(lat), Some(lon)) => Some((Station(stn, wban), Location(lat, lon)))
        case _ => None
      }
    } else {
      None
    }

  }

  lazy val stationMapping: Map[Station, Location] = getStations("/stations.csv")

  def readLines(file: String): Iterator[String] = {
    val stream = getClass.getResourceAsStream(file)
    scala.io.Source.fromInputStream(stream).getLines
  }

  def farenheitToCelcius(temperature: Double): Double =
    (temperature - 32.0) * 5.0 / 9.0

  def getStations(stationsFile: String): Map[Station, Location] = {
    readLines(stationsFile)
      .map(parseStation(_))
      .filter({
        case Some(_) => true
        case _ => false
      })
      .map(v => {
        val (station, location) = v.get
        (station -> location)
      })
      .toMap
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    def helper (): Iterator[Option[(LocalDate, Location, Double)]] = {
      val stations = getStations(stationsFile)
      val readings = readLines(temperaturesFile)
      for (reading <- readings) yield {
        val values = reading.split(",").map(_.trim)
        if (values.length == 5) {
          val stn = if (values(0) != "") values(0).toInt else 0
          val wban = if (values(1) != "") values(1).toInt else 0
          val month = if (values(2) != "") values(2).toInt else 0
          val day = if (values(3) != "") values(3).toInt else 0
          val tempF = if (values(4) != "") values(4).toDouble else 0.0
          val date = LocalDate.of(year, month, day)
          val tempC = farenheitToCelcius(tempF)
          val location = stations.get(Station(stn, wban))
          location.map(loc => (date, loc, tempC))
        } else {
          None
        }
      }
    }
    helper.filter(_ match {
      case Some(r) => true
      case _ => false
    }).flatten.toSeq
  }

  def locationTemperatures(year: Int): Iterable[(LocalDate, Location, Double)] = {

    def parseTemperatureReading(reading: String): Option[(LocalDate, Location, Double)] = {
      val values = reading.split(",").map(_.trim)
      if (values.length == 5) {
        val stn = if (values(0) != "") values(0).toInt else 0
        val wban = if (values(1) != "") values(1).toInt else 0
        val month = if (values(2) != "") values(2).toInt else 0
        val day = if (values(3) != "") values(3).toInt else 0
        val tempF = if (values(4) != "") values(4).toDouble else 0.0
        val date = LocalDate.of(year, month, day)
        val tempC = farenheitToCelcius(tempF)
        val location = stationMapping.get(Station(stn, wban))
        location match {
          case Some(loc) => Some((date, loc, tempC))
          case _ => None
        }
      } else {
        None
      }
    }

    readLines(s"/${year}.csv")
      .map(parseTemperatureReading(_))
      .filter({
        case Some(_) => true
        case _ => false
      })
      .flatten
      .toSeq
  }

  def locationYearlyAverages(file: String): Iterable[(Location, Double)] = {
    readLines(file)
      .map(line => {
        val values = line.split(",").map(_.trim)
        val lat = values(0).toDouble
        val lon = values(1).toDouble
        val t = values(2).toDouble
        (Location(lat, lon), t)
      })
      .toSeq
  }

  def locationAverages(records: Iterable[(Location, Double)]): Iterable[(Location, Double)] = {
    println(s"${java.time.LocalTime.now}: locationAverages...")
    val result =
      records.foldLeft(Map[Location, Double]())((mappings: Map[Location, Double], record: (Location, Double)) => {
            val (l, t) = record
            val mapping = mappings.get(l)
            mapping match {
              case Some(entry) => mappings + (l -> (entry + t) / 2)
              case _ => mappings + (l -> t)
            }
          })
    println(s"${java.time.LocalTime.now}: locationAverages...completed")
    result
  }

  def average(entries: Iterable[(LocalDate, Location, Double)]): Double = {
    val temp = entries.map(kv => kv._3).toList
    temp.sum / temp.length
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val result = records
      .groupBy(t => t._2)
      .map(kv => (kv._1, average(kv._2)))
      result
  }

  def locationYearlyAverageRecords3(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val result =
      records.foldLeft(Map[Location, Double]())((mappings: Map[Location, Double], record: (LocalDate, Location, Double)) => {
            val (_, l, t) = record
            val mapping = mappings.get(l)
            mapping match {
              case Some(entry) => mappings + (l -> (entry + t) / 2)
              case _ => mappings + (l -> t)
            }
          })
    result
  }
}
