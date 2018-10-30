package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._

  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    downsweepSequential(Array[Float](0f, 7f, 5f, 33f, 48f), output, 0f, 0, output.length)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }

  test("up and down should correctly handle a 4 element array when the starting angle is zero") {
    val input = Array[Float](0f, 7f, 5f, 33f, 48f)
    val output = new Array[Float](input.length)
    val t = upsweep(input, 0, input.length, 1)
    downsweep(input, output, 0, t)
    assert(output.toList == List(0f, 7f, 7f, 11f, 12f))
  }

}

