package relationalClustering.bagComparison

/**
  * Created by seb on 03.02.16.
  */
abstract class AbstractBagCompare(protected val identity: String) {


  /** Returns the identity of the bag compare function */
  def name = {
    identity
  }

  /** Takes two bags and compares them
    *
    * @param bag1 list of elements of the first bag
    * @param bag2 list of elements of the second bag
    * */
  def compareBags(bag1: List[String], bag2: List[String]): Double

  /** Should measure be normalized and inverted to be turned into a similarity measure */
  def needsToBeInverted : Boolean

  /** Extract the union of all elements in two bags; in case there are no element, put a fake element in a bag to avoid problems later
    *
    * @param bag1 list of all elements in bag one
    * @param bag2 list of all elements in bag two
    * @return list of all common elements
    * */
  protected def commonElements(bag1: List[String], bag2: List[String]) = {
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
  protected def bagToHistogram(bag: List[String], histogramElements: List[String]) = {
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
