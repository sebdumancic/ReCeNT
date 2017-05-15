package relationalClustering.bags.bagCombination

/**
  * Created by seb on 11.02.16.
  */
class UnionCombination extends AbstractBagCombine("union") {

  def combineBags[T](bag1: Map[T, Int], bag2: Map[T, Int]): Map[T, Int] = {
    bag1.keySet.union(bag2.keySet).map(key => (key, bag1.getOrElse(key, 0) + bag2.getOrElse(key, 0))).toMap
  }
}
