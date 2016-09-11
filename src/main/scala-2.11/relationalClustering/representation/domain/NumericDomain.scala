package relationalClustering.representation.domain

/**
  * Created by seb on 11/09/16.
  */
class NumericDomain(override protected val name: String) extends Domain(name) {

  protected var minElement: Double = Double.NaN
  protected var maxElement: Double = Double.NaN

  override def hasElement(element: String) = {
    if (minElement.isNaN || maxElement.isNaN) {
      false
    }
    else {
      element.toDouble >= minElement && element.toDouble <= maxElement
    }
  }

  override def addElement(elem: String) = {
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

  def getRange = {
    maxElement - minElement
  }

  override def size = {
    getRange
  }

  override def removeElement(elem: String): Unit = {
    throw  new Exception(s"NumericDomain::removeElement: not possible to call this method for numeric domains!")
  }

}
