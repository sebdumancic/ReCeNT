package relationalClustering.bagComparison.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class UnionCombination extends AbstractBagCombine("union") {

  def combineBags[T](bag1: List[T], bag2: List[T]) = {
    bag1.union(bag2)
  }
}
