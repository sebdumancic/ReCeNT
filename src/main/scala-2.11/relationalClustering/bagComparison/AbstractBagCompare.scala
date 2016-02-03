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
}
