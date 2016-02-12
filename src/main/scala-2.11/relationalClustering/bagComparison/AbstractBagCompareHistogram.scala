package relationalClustering.bagComparison

/**
  * Created by seb on 03.02.16.
  */
abstract class AbstractBagCompareHistogram(override protected val identity: String) extends AbstractBagComparison(identity) {

  /** Takes two bags and compares them
    *
    * @param bag1 list of elements of the first bag
    * @param bag2 list of elements of the second bag
    * */
  def compareBags[T](bag1: List[T], bag2: List[T]): Double = {
    val histElements = commonElements(bag1, bag2)

    similarity(bagToHistogram(bag1, histElements), bagToHistogram(bag2, histElements))
  }

  /** Implements a desired similarity measure over histograms
    *
    * @param hist1 normalized histogram
    * @param hist2 normalized histogram
    * */
  protected def similarity(hist1: List[Double], hist2: List[Double]) : Double

  /** Extract the union of all elements in two bags; in case there are no element, put a fake element in a bag to avoid problems later
    *
    * @param bag1 list of all elements in bag one
    * @param bag2 list of all elements in bag two
    * @return list of all common elements
    * */
  protected def commonElements[T](bag1: List[T], bag2: List[T]) = {
    val distinctElements = bag1.toSet.union(bag2.toSet)

    distinctElements.isEmpty match {
      case true => List[String]("fakeElement")
      case false => distinctElements.toList
    }
  }

  /** Turns bag into a normalized histogram
    *
    * @param bag bag to be turned into histogram
    * @param histogramElements ordered elements in the histogram
    * @return list[Double]
    * */
  protected def bagToHistogram[T](bag: List[T], histogramElements: List[T]) = {
    normalizeBySum(histogramElements.map(x => bag.count( _ == x))).map(x => if (x.isNaN) 0.0 else x)
  }

  /** Normalizing histogram by the total number of elements
    *
    * @param histogram ordered list of element counts
    * */
  protected def normalizeBySum(histogram: List[Int]) = {
    val normConstant = histogram.sum
    histogram.map( _.toDouble / normConstant)
  }
}
