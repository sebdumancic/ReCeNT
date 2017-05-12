package relationalClustering.bags.bagComparison

/**
  * Created by seb on 15.03.16.
  */
class SimpleCountSimilarity extends AbstractBagComparison("simpleCount") {

  def needsToBeInverted: Boolean = {
    false
  }

  def compareBags[T](bag1: Map[T, Int], bag2: Map[T, Int]): Double = {
    val lBag1 = bag1.foldLeft(List[T]())((acc, elem) => {
      acc ++ (0 to elem._2).toList.map(i => elem._1)
    })
    val lBag2 = bag2.foldLeft(List[T]())((acc, elem) => {
      acc ++ (0 to elem._2).toList.map(i => elem._1)
    })
    val value = lBag1.intersect(lBag2).size.toDouble

    if (value.isNaN) {
      value
    }
    else {
      0.0
    }

  }
}
