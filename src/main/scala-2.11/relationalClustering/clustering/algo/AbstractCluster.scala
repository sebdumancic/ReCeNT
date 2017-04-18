package relationalClustering.clustering.algo

import relationalClustering.representation.clustering.Clustering
import relationalClustering.similarity.AbstractSimilarityNTrees

/**
  * Created by seb on 04.02.16.
  */
abstract class AbstractCluster() {


  def clusterVertices(domains: List[String], similarityMeasure: AbstractSimilarityNTrees, k: Int, baseOffset: Int): Clustering

  def clusterEdges(domains: List[String], similarityMeasure: AbstractSimilarityNTrees, k: Int, baseOffset: Int): Clustering
}
