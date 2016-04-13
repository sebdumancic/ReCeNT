package relationalClustering.bagComparison

/**
  * Created by seb on 12.02.16.
  */
class MaximumSimilarity extends AbstractBagComparison("max") {

  def needsToBeInverted = {
    false
  }

  def compareBags[T](bag1: List[T], bag2: List[T]) = {
    val value = bag1.intersect(bag2).size.toDouble/math.max(bag1.size, bag2.size).toDouble
    value.isNaN match {
      case true => 0.0
      case false => value
    }

  }
}
