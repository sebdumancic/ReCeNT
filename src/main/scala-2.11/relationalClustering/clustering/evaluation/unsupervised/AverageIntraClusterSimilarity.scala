package relationalClustering.clustering.evaluation.unsupervised

import breeze.linalg.DenseMatrix
import relationalClustering.representation.clustering.{Cluster, Clustering}

import scala.io.Source

/**
  * Created by seb on 19.02.16.
  */
class AverageIntraClusterSimilarity extends AbstractEvaluatorModel("./tmp") {

  /** Calculates the average intra cluster similarity */
  def validate(clusters: Clustering) = {
    val similarityMatrix = readMatrixFromFile(clusters.getSimilarityFilename, clusters.getElementOrdering.length)
    val agg = clusters.getClusters.map( clust => intraClusterSimilarity(clust, clusters.getElementOrdering.map(_.mkString(":")), similarityMatrix)) //.sum/clusters.size.toDouble
    agg.sum //clusters.size.toDouble
  }

  /** Calculates the intra cluster similarity
    *
    * @param cluster a list of elements in a cluster
    * @param elementOrder order of the elements in the similarity matrix
    * @param similarityMatrix similarity matrix
    * @return [[Double]]
    * */
  protected def intraClusterSimilarity(cluster: Cluster, elementOrder: List[String], similarityMatrix: DenseMatrix[Double]): Double = {
    cluster.getSize < 2 match {
      case true => 0.0
      case false =>
        cluster.getInstances.map(_.mkString(":")).toList.zipWithIndex.foldLeft(0.0)((acc, elem) => {
          acc + cluster.getInstances.map(_.mkString(":")).toList.zipWithIndex.filter( _._2 > elem._2).foldLeft(0.0)( (acc_i, elem_i) => {
            acc_i + similarityMatrix(elementOrder.indexOf(elem._1), elementOrder.indexOf(elem_i._1))
          })
        }) / ((cluster.getSize * (cluster.getSize - 1)).toDouble / 2)
    }
  }

  /** Reads a matrix from file
    *
    * @param similarityMatrixFile file containing the similarity matrix
    * @param numElements number of elements in a matrix (column and row size)
    * @return [[DenseMatrix]]
    * */
  protected def readMatrixFromFile(similarityMatrixFile: String, numElements: Int) = {
    val mat = DenseMatrix.zeros[Double](numElements, numElements)

    val file = Source.fromFile(similarityMatrixFile)

    try {
      var lineIndex = 0
      file.getLines().foreach( line => {
        if (!line.startsWith("#")) {
          line.split(";").map(_.toDouble).zipWithIndex.foreach(rowElem => {
            mat(lineIndex, rowElem._2) = rowElem._1
          })

          lineIndex += 1
        }
      })
    }
    finally {
      file.close()
    }

    mat
  }
}
