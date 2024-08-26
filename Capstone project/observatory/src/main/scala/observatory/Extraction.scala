package observatory

import java.time.LocalDate
import scala.io.Source
/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
    // Load and parse the stations file
    val stations = Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines()
      .flatMap { line =>
        val parts = line.split(",")
        if parts.length >= 4 && parts(2).nonEmpty && parts(3).nonEmpty then
          val stnId = if parts(0).isEmpty then None else Some(parts(0))
          val wbanId = if parts(1).isEmpty then None else Some(parts(1))
          val location = Location(parts(2).toDouble, parts(3).toDouble)
          Some(((stnId, wbanId), location))
        else None
      }.toMap

    // Load and parse the temperatures file
    val temperatures = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile)).getLines()
      .flatMap { line =>
        val parts = line.split(",")
        val stnId = if parts(0).isEmpty then None else Some(parts(0))
        val wbanId = if parts(1).isEmpty then None else Some(parts(1))
        stations.get((stnId, wbanId)).map { location =>
          val month = parts(2).toInt
          val day = parts(3).toInt
          val tempInFahrenheit = parts(4).toDouble
          val tempInCelsius = (tempInFahrenheit - 32) * 5.0 / 9.0
          (LocalDate.of(year, month, day), location, tempInCelsius)
        }
      }.toList

    temperatures

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    records.groupBy(_._2)
      .map { case (location, temps) =>
        val avgTemp = temps.map(_._3).sum / temps.size
        (location, avgTemp)
      }.toList

