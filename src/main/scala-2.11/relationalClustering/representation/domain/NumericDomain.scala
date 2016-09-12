package relationalClustering.representation.domain

import breeze.linalg.DenseVector
import breeze.stats.stddev

/**
  * Created by seb on 11/09/16.
  */
class NumericDomain(override protected val name: String) extends Domain(name) {

  protected var minElement: Double = Double.NaN
  protected var maxElement: Double = Double.NaN

  /** Checks whether an element is in the interval of the domains
    *
    * @param element element to check
    */
  override def hasElement(element: String) = {
    if (minElement.isNaN || maxElement.isNaN) {
      false
    }
    else {
      element.toDouble >= minElement && element.toDouble <= maxElement
    }
  }

  /** Adds an element to a domain: increases the upper bound is necessary, lowers the lower bound if element outside of the range*/
  override def addElement(elem: String) = {
    elements += elem
    if (minElement.isNaN || maxElement.isNaN) {
      minElement = elem.toDouble
      maxElement = elem.toDouble
    }
    else if (elem.toDouble < minElement) {
      minElement = elem.toDouble
    }
    else if (elem.toDouble > maxElement) {
      maxElement = elem.toDouble
    }
    elements // just for compatibility with the Domain
  }

  /** Returns the range of the interval representing the domain */
  def getRange = {
    maxElement - minElement
  }

  /** Returns the standard deviation over the entire domain */
  def getStdDev = {
    val dElements = elements.toList.map(_.toDouble)
    stddev(DenseVector.tabulate(dElements.length){ i => dElements(i)})
  }

  override def size = {
    getRange
  }

  override def removeElement(elem: String): Unit = {
    throw  new Exception(s"NumericDomain::removeElement: not possible to call this method for numeric domains!")
  }

}
