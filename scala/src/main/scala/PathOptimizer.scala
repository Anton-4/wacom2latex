import scala.collection.mutable.ListBuffer

/**
  * Created by anton on 15.07.17.
  */
object PathOptimizer {

  def smartSubsetPoints(points: ListBuffer[Point], subsetSize: Int) = {
    while (points.size > subsetSize){
      val pair = ClosestPair.divideAndConquer(points.toList)
      val removePoint = pair.point2
      points -= removePoint
    }

    while(points.size < 32){
      points += Point(-1.0, -1.0)
    }

    points
  }

}
