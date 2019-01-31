package stackoverflow

import org.apache.spark.rdd.RDD
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite};
import org.apache.spark.util.collection.CompactBuffer

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("grouped postings") {
    val pos1 = Posting(postingType = 1,
      id = 1,
      acceptedAnswer = Some(2),
      parentId = None,
      score = 3,
      tags = None)
    val pos2 = Posting(postingType = 2,
      id = 2,
      acceptedAnswer = None,
      parentId = Some(1),
      score = 4,
      tags = None)
    val pos3 = Posting(postingType = 2,
      id = 3,
      acceptedAnswer = None,
      parentId = Some(1),
      score = 5,
      tags = None)
    val pos4 = Posting(postingType = 1,
      id = 4,
      acceptedAnswer = None,
      parentId = None,
      score = 1,
      tags = None)

    val postingList = List(pos1, pos2, pos3, pos4)

    val postings: RDD[Posting] = StackOverflow.sc.parallelize(postingList)

    val res = testObject.groupedPostings(postings).collect()

    val exp = Array((1, List((pos1,pos2), (pos1, pos3))))

    assert(res sameElements exp)
  }

}
