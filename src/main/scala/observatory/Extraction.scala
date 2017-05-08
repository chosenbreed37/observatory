package observatory

import java.time.LocalDate
import java.lang.{Class, ClassLoader}
import fs2.{io, text, Task}
import scala._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  def readLines(file: String): Iterator[String] = {
    val stream = getClass.getResourceAsStream(file)
    scala.io.Source.fromInputStream(stream).getLines
  }

  def farenheitToCelcius(temperature: Double): Double =
    (temperature - 32.0) * 5.0 / 9.0

  def getStations(stationsFile: String): scala.collection.mutable.Map[Station, Location] = {
    var stations = scala.collection.mutable.Map[Station, Location]()
    val lines = readLines(stationsFile)
      for (line <- lines) {
      val values = line.split(",").map(_.trim)
      if (values.length == 4) {
        val stn = if (values(0) != "") values(0).toInt else 0
        val wban = if (values(1) != "") values(1).toInt else 0
        val latOp = if (values(2) != "") Some(values(2).toDouble) else None
        val lonOp = if (values(3) != "") Some(values(3).toDouble) else None
        (latOp, lonOp) match {
          case (Some(lat), Some(lon)) =>
            stations += (Station(stn, wban) -> Location(lat, lon))
          case _ => Unit
        }
      }
    }
    stations
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    //
    // val readings = io.Source.fromFile(temparaturesFiles).getLines
    // for (reading <- readings) {
    //
    // }
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

  def average(entries: Iterable[(LocalDate, Location, Double)]): Double = {
    val temp = entries.map(kv => kv._3).toList
    temp.sum / temp.length
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(t => t._2)
      .map(kv => (kv._1, average(kv._2)))
  }

}
