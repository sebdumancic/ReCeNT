package relationalClustering.clustering.evaluation

import java.io.FileWriter

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 09.02.16.
  */
abstract class AbstractEvaluatorModel(override protected val rootFolder: String) extends AbstractEvaluator(rootFolder) {

  /** Performs model-based evaluation of clusters
    *
    * @param clusters clusters to evaluate
    * */
  def validate(clusters: Clustering): Double

  /** Turns a list of examples into cluster labels
    *
    * @param clusters a set of clusters
    * @param elementOrder an ordered list of elements
    * @return list of cluster labels corresponding to the provided list of elements
    * */
  protected def getLabels(clusters: Clustering, elementOrder: List[String]) = {
    val exampleToLabels = collection.mutable.Map[String, Int]()

    clusters.getClusters.zipWithIndex.foreach( clust => {
      clust._1.getInstances.map(_.mkString(":")).toList.foreach( elem => exampleToLabels(elem) = clust._2 )
    })

    elementOrder.map( el => exampleToLabels(el) )
  }

  /** Saves labels to a file
    *
    * @param labels list of label identifiers (Int)
    * @param filename name of the file
    * */
  protected def saveLabelsToFile(labels: List[Int], filename: String) = {
    val writer = new FileWriter(s"$getRoot/$filename")
    try {
      writer.write(labels.mkString("\n") + "\n")
    }
    finally {
      writer.close()
    }
  }
}
