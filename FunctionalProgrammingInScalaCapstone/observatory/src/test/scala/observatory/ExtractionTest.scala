package observatory

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  val testObject = Extraction

  test("wholeTextFiles") {
    val record = testObject.locateTemperatures(1990, "/stations.csv", "/1990.csv")
    testObject.locationYearlyAverageRecords(record)
  }
}