/**
  * Created by anton on 15.07.17.
  */
import scala.collection.mutable.ListBuffer
import scala.util.Random

object ClosestPair {
  case class Pair(point1: Point, point2: Point) {
    val distance: Double = point1 distanceTo point2

    override def toString = {
      point1 + "-" + point2 + " : " + distance
    }
  }

  private def sortByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  private def sortByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }

  def divideAndConquer(points: List[Point]): Pair = {
    val pointsSortedByX = sortByX(points)
    val pointsSortedByY = sortByY(points)

    divideAndConquer(pointsSortedByX, pointsSortedByY)
  }

  def bruteForce(points: List[Point]): Pair = {
    val numPoints = points.size
    if (numPoints < 2)
      return null
    var pair = Pair(points(0), points(1))
    if (numPoints > 2) {
      for (i <- 0 until numPoints - 1) {
        val point1 = points(i)
        for (j <- i + 1 until numPoints) {
          val point2 = points(j)
          val distance = point1 distanceTo point2
          if (distance < pair.distance)
            pair = Pair(point1, point2)
        }
      }
    }
    return pair
  }


  private def divideAndConquer(pointsSortedByX: List[Point], pointsSortedByY: List[Point]): Pair = {
    val numPoints = pointsSortedByX.size
    if(numPoints <= 3) {
      return bruteForce(pointsSortedByX)
    }

    val dividingIndex = numPoints >>> 1
    val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
    val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

    var tempList = leftOfCenter.map(x => x)
    //println(tempList)
    tempList = sortByY(tempList)
    var closestPair = divideAndConquer(leftOfCenter, tempList)

    tempList = rightOfCenter.map(x => x)
    tempList = sortByY(tempList)

    val closestPairRight = divideAndConquer(rightOfCenter, tempList)

    if (closestPairRight.distance < closestPair.distance)
      closestPair = closestPairRight

    tempList = List[Point]()
    val shortestDistance = closestPair.distance
    val centerX = rightOfCenter(0).x

    for (point <- pointsSortedByY) {
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList = tempList :+ point
    }

    closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
    closestPair
  }

  private def shortestDistanceF(tempList: List[Point], shortestDistance: Double, closestPair: Pair ): Pair = {
    var shortest = shortestDistance
    var bestResult = closestPair
    for (i <- 0 until tempList.size) {
      val point1 = tempList(i)
      for (j <- i + 1 until tempList.size) {
        val point2 = tempList(j)
        if ((point2.y - point1.y) >= shortestDistance)
          return closestPair
        val distance = point1 distanceTo point2
        if (distance < closestPair.distance)
        {
          bestResult = Pair(point1, point2)
          shortest = distance
        }
      }
    }

    closestPair
  }
}
