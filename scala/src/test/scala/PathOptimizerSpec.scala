import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

/**
  * Created by anton on 15.07.17.
  */
class PathOptimizerSpec extends FlatSpec with Matchers{
  "PathOptimizer" should "only leav the most distant points" in {
    val points = ListBuffer[Point]()
    points += Point(1.0,1.0)
    points += Point(2.0,2.0)
    points += Point(4.0,4.0)
    points += Point(6.0,6.0)
    points += Point(7.0,7.0)
    points += Point(9.0,9.0)
    points += Point(11.0,11.0)
    points += Point(13.0,13.0)

    val subset = PathOptimizer.smartSubsetPoints(points, 6)

    subset.size should be (6)
  }
}
