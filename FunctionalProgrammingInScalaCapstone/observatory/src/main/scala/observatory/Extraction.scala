package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
  * 1st milestone: data extraction
  */
object Extraction {

  @transient lazy val spark: SparkSession = SparkSession.builder.master("local").appName("Observatory").getOrCreate()
  @transient lazy val sc: SparkContext = spark.sparkContext

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val temps = sc.textFile(Paths.get(getClass.getResource(temperaturesFile).toURI).toString)
    val stations = sc.textFile(Paths.get(getClass.getResource(stationsFile).toURI).toString)

    val pTemps = parseTemps(temps, year)
    val pStations = parseStations(stations)

    pTemps.join(pStations).map(pair => {
      val ((ld, t), lo) = pair._2
      (ld, lo, t)
    }).collect()
  }

  def parseTemps(temps: RDD[String], year: Year): RDD[((String, String), (LocalDate, Temperature))] = {
    temps.flatMap(line => {
      val arr = line.split(",")

      arr match {
        case Array(s, w, m, d, f) if (s != "" || w != "") && f != "9999.9" => {
          val c = (f.toDouble - 32) * (5.0 / 9.0)
          Some(((s, w), (LocalDate.of(year, m.toInt, d.toInt), c)))
        }
        case _ => None
      }
    })

  }

  def parseStations(stations: RDD[String]): RDD[((String, String), Location)] = {
    stations.flatMap(line => {
      val arr = line.split(",")

      arr match {
        case Array(s, w, la, lo) if (s != "" || w != "") && (la != "" && lo != "") => Some(((s, w), Location(la.toDouble, lo.toDouble)))
        case _ => None
      }
    })
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val rdd = sc.parallelize(records.toList).map(r => (r._2, (1, r._3)))

    rdd.reduceByKey((x, y) => (x._1 + y._1, x._2 + y._2)).mapValues(p => p._2 / p._1).collect()
  }

}
