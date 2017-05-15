package relationalClustering.bags.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class IntersectionCombination extends AbstractBagCombine("intersection") {

  def combineBags[T](bag1: Map[T, Int], bag2: Map[T, Int]): Map[T, Int] = {
    bag1.keySet.intersect(bag2.keySet).map(key => (key, math.min(bag1(key), bag2(key)))).toMap
  }
}
