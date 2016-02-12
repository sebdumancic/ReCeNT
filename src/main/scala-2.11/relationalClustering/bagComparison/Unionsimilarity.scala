package relationalClustering.bagComparison

/**
  * Created by seb on 12.02.16.
  */
class Unionsimilarity extends AbstractBagComparison("union"){

  def needsToBeInverted = {
    false
  }

  def compareBags[T](bag1: List[T], bag2: List[T]) = {
    bag1.intersect(bag2).size.toDouble/bag1.union(bag2).size.toDouble
  }
}
