package relationalClustering.bagComparison

/**
  * Created by seb on 15.03.16.
  */
class SimpleCountSimilarity extends AbstractBagComparison("simpleCount") {

  def needsToBeInverted = {
    false
  }

  def compareBags[T](bag1: List[T], bag2: List[T]) = {
    bag1.intersect(bag2).size.toDouble
  }
}
