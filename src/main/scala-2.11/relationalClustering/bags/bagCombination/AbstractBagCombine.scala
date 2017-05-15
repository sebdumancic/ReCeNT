package relationalClustering.bags.bagCombination

/**
  * Created by seb on 11.02.16.
  */
abstract class AbstractBagCombine(protected val identity: String) {

  /** Returns the name of the combination function */
  def getName: String = {
    identity
  }

  /** Combines the provided bags in a single one
    *
    * @param bag1 the first bag of elements
    * @param bag2 the second bag of elements
    * @return bag combination
    * */
  def combineBags[T](bag1: Map[T, Int], bag2: Map[T, Int]): Map[T, Int]
}
