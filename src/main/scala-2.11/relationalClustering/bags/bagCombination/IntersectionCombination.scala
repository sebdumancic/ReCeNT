package relationalClustering.bags.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class IntersectionCombination extends AbstractBagCombine("intersection") {

  def combineBags[T](bag1: List[T], bag2: List[T]) = {
    bag1.intersect(bag2)
  }
}
