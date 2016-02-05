package relationalClustering.clustering.evaluation

import relationalClustering.utils.Helper

/**
  * Created by seb on 05.02.16.
  */
class LabelsContainer(private val filename: String) {

  // contains labels of objects: object name -> labels
  private val labels = collection.mutable.Map[String, String]()
  private val labelsRegex = """(.*?)\((.*?)\)""".r
  private var allLabels = List[String]()
  readLabels()

  /** Read the file containing labels */
  private def readLabels() = {
    Helper.readFile(filename).filterNot( x => x.length < 2 || x.startsWith("//") || x.startsWith("#")).foreach( line =>{
      val labelsRegex(label, element) = line
      labels(element) = label

      if (!allLabels.contains(label)) {
        allLabels = allLabels :+ label
      }
    })
  }

  /** Returns the label of the element
    *
    * @param element name of the element
    * @return element's label: [[String]]
    * */
  def getLabel(element: String) = {
    labels(element)
  }

  /** Returns the label id for the element in question
    *
    * @param element name of the element
    * @return label id [[Int]]
    * */
  def getLabelId(element: String) = {
    allLabels.indexOf(getLabel(element))
  }
}
