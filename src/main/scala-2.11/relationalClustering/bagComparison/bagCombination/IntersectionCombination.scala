package relationalClustering.bagComparison.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class IntersectionCombination extends AbstractBagCombine("intersection") {

  def combineBags(bag1: List[String], bag2: List[String]) = {
    bag1.intersect(bag2)
  }
}
