package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    (lat, lon) => {
      Visualization.predictTemperature(temperatures, Location(lat, lon))
    }
  }

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    (lat, lon) => {
      val (total, count) = temperatures.foldLeft(0d, 0)((acc, curr) => {
        val (runningTotal, runningCount) = acc
        val t = makeGrid(curr)(lat, lon)
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
    (lat, lon) => {
      val c = makeGrid(temperatures)(lat, lon)
      val r = normals(lat, lon)
      c - r
    }
  }


}
