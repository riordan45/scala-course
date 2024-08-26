package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given
import scala.collection.parallel.CollectionConverters.given

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    // temperatures.groupBy(_._1).getOrElse(location, Nil).head._2
    temperatures.collectFirst {
    case (loc, temp) if greatCircleDistance(loc, location) < 1.0 => temp
    }.getOrElse {
      val p = 2
    // If no very close location, perform inverse distance weighting
      val weightedTemperatures = temperatures.map { case (loc, temp) =>
      val distance = greatCircleDistance(location, loc)
      val weight = 1.0 / math.pow(distance, p)
      (weight * temp, weight)
    }

      val (weightedSum, weightSum) = weightedTemperatures.reduce { (a, b) =>
      (a._1 + b._1, a._2 + b._2)
    }

      weightedSum / weightSum
    }


  // Helper function to calculate the great-circle distance between two locations
  def greatCircleDistance(loc1: Location, loc2: Location): Double = 
    val earthRadius = 6371.0 // Earth radius in kilometers

    val lat1 = math.toRadians(loc1.lat)
    val lon1 = math.toRadians(loc1.lon)
    val lat2 = math.toRadians(loc2.lat)
    val lon2 = math.toRadians(loc2.lon)

    val dLat = lat2 - lat1
    val dLon = lon2 - lon1

    val a = math.sin(dLat / 2) * math.sin(dLat / 2) +
      math.cos(lat1) * math.cos(lat2) *
      math.sin(dLon / 2) * math.sin(dLon / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))

    earthRadius * c
  
    

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
      val sortedPoints = points.toSeq.sortBy(_._1)

  // If the value matches exactly one of the points
      sortedPoints.find(_._1 == value).map(_._2).getOrElse {
    // Find the two closest points between which the value lies
    sortedPoints.zip(sortedPoints.tail).find { case ((t1, _), (t2, _)) =>
      t1 <= value && value <= t2
    } match {
      case Some(((t1, c1), (t2, c2))) =>
        val factor = (value - t1) / (t2 - t1)
        Color(
          red = (c1.red + ((c2.red - c1.red) * factor).round).toInt,
          green = (c1.green + ((c2.green - c1.green) * factor).round).toInt,
          blue = (c1.blue + ((c2.blue - c1.blue) * factor).round).toInt
        )
      case None =>
        if (value < sortedPoints.head._1) sortedPoints.head._2
        else sortedPoints.last._2
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =
    val width = 360
    val height = 180

    val pixels = (0 until height).par.flatMap { y =>
      (0 until width).par.map { x =>
        val lon = x - 180
        val lat = 90 - y
        val location = Location(lat, lon)

        // Predict the temperature at this location
        val temp = predictTemperature(temperatures, location)

        // Find the color corresponding to this temperature
        val color = interpolateColor(colors, temp)

        // Create the pixel with the computed color
        Pixel(x, y, color.red, color.green, color.blue, 255)
      }
    }

    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)


