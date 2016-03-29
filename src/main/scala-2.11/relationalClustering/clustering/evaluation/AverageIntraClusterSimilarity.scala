package relationalClustering.clustering.evaluation

import breeze.linalg.DenseMatrix

import scala.io.Source

/**
  * Created by seb on 19.02.16.
  */
class AverageIntraClusterSimilarity extends AbstractEvaluatorModel("./tmp") {

  /** Calculates the average intra cluster similarity */
  def validate(clusters: Set[List[String]], elementOrder: List[String], similarityMatrixFile: String) = {
    val similarityMatrix = readMatrixFromFile(similarityMatrixFile, elementOrder.length)
    val agg = clusters.map( clust => intraClusterSimilarity(clust, elementOrder, similarityMatrix)) //.sum/clusters.size.toDouble
    agg.sum///clusters.size.toDouble
  }

  /** Calculates the intra cluster similarity
    *
    * @param cluster a list of elements in a cluster
    * @param elementOrder order of the elements in the similarity matrix
    * @param similarityMatrix similarity matrix
    * @return [[Double]]
    * */
  protected def intraClusterSimilarity(cluster: List[String], elementOrder: List[String], similarityMatrix: DenseMatrix[Double]): Double = {
    cluster.length < 2 match {
      case true => 0.0
      case false =>
        (2.0/(cluster.length*(cluster.length - 1)).toDouble) * cluster.zipWithIndex.foldLeft(0.0)( (acc, elem) => {
          acc + cluster.zipWithIndex.filter( _._2 > elem._2).foldLeft(0.0)( (acc_i, elem_i) => {
            acc_i + similarityMatrix(elementOrder.indexOf(elem._1), elementOrder.indexOf(elem_i._1))
          })
        })
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
