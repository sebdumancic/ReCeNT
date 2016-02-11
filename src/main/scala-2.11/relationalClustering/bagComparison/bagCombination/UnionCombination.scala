package relationalClustering.bagComparison.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class UnionCombination extends AbstractBagCombine("union") {

  def combineBags(bag1: List[String], bag2: List[String]) = {
    bag1.union(bag2)
  }
}
