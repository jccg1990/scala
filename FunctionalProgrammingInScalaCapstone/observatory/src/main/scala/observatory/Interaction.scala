package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    def positionToLocation(p: Int) = {
      val x1 = 256 * tile.x + p % 256
      val y1 = 256 * tile.y + p / 256

      tileLocation(Tile(x1, y1, tile.zoom + 8))
    }

    val pixels = (0 until 256 * 256).par.map(p => {
      val t = predictTemperature(temperatures, positionToLocation(p))
      val c = interpolateColor(colors, t)
      Pixel(c.red, c.green, c.blue, 127)
    })

    Image(256, 256, pixels.toArray)
  }

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    def toLocation(x: Int, y: Int, z: Int) = Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1 << z))))),
      x.toDouble / (1 << z) * 360.0 - 180.0)

    toLocation(tile.x, tile.y, tile.zoom)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Year, Data)], generateImage: (Year, Tile, Data) => Unit): Unit = {
    for {
      z <- 0 to 3
      x <- 0 until 1 << z
      y <- 0 until 1 << z
      (year, d) <- yearlyData
    } yield generateImage(year, Tile(x, y, z), d)
  }

}
