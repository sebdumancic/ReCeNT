package relationalClustering.bagComparison

/**
  * Created by seb on 12.02.16.
  */
class MinimumSimilarity extends AbstractBagComparison("min") {

  def needsToBeInverted = {
    false
  }

  def compareBags[T](bag1: List[T], bag2: List[T]) = {
    bag1.intersect(bag2).size.toDouble/math.min(bag1.size, bag2.size).toDouble
  }

}
