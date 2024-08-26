package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    val n = math.pow(2.0, tile.zoom)
    val lon = tile.x / n * 360.0 - 180.0
    val lat = math.atan(math.sinh(math.Pi * (1 - 2 * tile.y / n))) * 180.0 / math.Pi
    Location(lat, lon)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage =
    val width = 256
    val height = 256

    val pixels = (0 until height).par.flatMap { y =>
      (0 until width).par.map { x =>
        val subTile = Tile(tile.x * 256 + x, tile.y * 256 + y, tile.zoom + 8)
        val location = tileLocation(subTile)

        // Predict the temperature at this location
        val temp = Visualization.predictTemperature(temperatures, location)

        // Find the color corresponding to this temperature
        val color = Visualization.interpolateColor(colors, temp)

        // Create the pixel with the computed color
        Pixel(x, y, color.red, color.green, color.blue, 255)
      }
    }

    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until math.pow(2, zoom).toInt
      y <- 0 until math.pow(2, zoom).toInt
    } {
      // Debugging statement to ensure tiles are being generated
      // println(s"Generating image for year $year at Tile($x, $y, $zoom)")
      generateImage(year, Tile(x, y, zoom), data)
    }

