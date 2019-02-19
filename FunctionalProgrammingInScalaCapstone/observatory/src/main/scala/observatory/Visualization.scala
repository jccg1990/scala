package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val p: Double = 2.0
  val earthRadius: Double = 6371.0088

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixels = (0 until 360 * 180).par.map(p => {
      val t = predictTemperature(temperatures, convertToLocation(p))
      val c = interpolateColor(colors, t)
      Pixel(c.red, c.green, c.blue, 255)
    })

    Image(360, 180, pixels.toArray)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def u: (Double, Double) = temperatures.par.map { case (l, t) =>
      def d: Double = calculateDistance(l, location)

      if (d < 1) {
        return (t, 1.0)
      } else {
        def wi: Double = 1.0 / pow(d, p)

        (wi * t, wi)
      }
    }.reduce((x, y) => (x._1 + y._1, x._2 + y._2))

    u._1 / u._2
  }

  def calculateDistance(x: Location, y: Location): Double = {
    def a = if (x.lat == y.lat && x.lon == y.lon) {
      0.0
    } else if (x.lat == -y.lat && (x.lon == y.lon - 180 || x.lon == y.lon + 180)) {
      math.Pi
    } else {
      val (la1, lo1) = (x.lat.toRadians, x.lon.toRadians)
      val (la2, lo2) = (y.lat.toRadians, y.lon.toRadians)

      acos(sin(la1) * sin(la2) + cos(la1) * cos(la2) * cos(abs(lo1 - lo2)))
    }

    a * earthRadius
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def findValue(points: Iterable[(Temperature, Color)]): Color = {
      points match {
        case x1 :: x2 :: _ =>
          if (x1._1 < value && x2._1 > value) {
            val r = lerp(x1._1, x2._1, x1._2.red, x2._2.red, value)
            val g = lerp(x1._1, x2._1, x1._2.green, x2._2.green, value)
            val b = lerp(x1._1, x2._1, x1._2.blue, x2._2.blue, value)

            Color(r, g, b)
          } else {
            findValue(points.tail)
          }
      }
    }

    val sorted = points.toList.sortBy(p => p._1)

    if (value <= sorted.head._1) {
      sorted.head._2
    } else if (value >= sorted.last._1) {
      sorted.last._2
    } else {
      findValue(sorted)
    }
  }

  def lerp(t0: Double, t1: Double, c0: Int, c1: Int, t: Double): Int = {
    min(255, (c0 + (t - t0) * (c1 - c0) / (t1 - t0)).round).toInt
  }

  def convertToLocation(idx: Int): Location = {
    val latitude: Int = ((idx / 360) * -1) + 90
    val longitude: Int = (idx % 360) - 180

    Location(latitude, longitude)
  }

}

