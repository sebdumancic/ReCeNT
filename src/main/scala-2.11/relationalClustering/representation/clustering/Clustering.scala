package relationalClustering.representation.clustering

import relationalClustering.similarity.AbstractSimilarityNTrees

/**
  * Created by seb on 17.03.16.
  */
class Clustering(protected val clusters: List[Cluster],
                 protected val similarityMeasure: AbstractSimilarityNTrees,
                 protected val elementOrdering: List[List[String]],
                 protected val similarityFilename: String) {

  def getClusters = {
    clusters
  }

  def getSimilarityFilename = {
    similarityFilename
  }

  def getElementOrdering = {
    elementOrdering
  }

}
