package relationalClustering.representation.clustering

import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.similarity.AbstractSimilarityNTrees

/**
  * Created by seb on 17.03.16.
  */
class Clustering(protected val clusters: List[Cluster],
                 protected val similarityMeasure: AbstractSimilarityNTrees,
                 protected val elementOrdering: List[List[String]],
                 protected val similarityFilename: String) {

  /** Returns individual clusters*/
  def getClusters = {
    clusters
  }

  /** Returns the filename containing the similarity matrix of elements in the clustering */
  def getSimilarityFilename = {
    similarityFilename
  }

  /** Returns the ordering of elements in a matrix*/
  def getElementOrdering = {
    elementOrdering
  }

  /** Returns the type of clusters within the clustering */
  def getTypes = {
    getClusters.head.getTypes
  }

  /** Assigns an object/hyper-edge to the closest cluster, according to the average distance to all elements in a cluster
    *
    * @param nt a list of neighbourhood trees representing an object/hyper-edge
    * @return the closest (most similar) cluster
    * */
  def assignToClosestClusterAverage(nt: List[NeighbourhoodGraph]) = {
    nt.length == 1 match {
      case true => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.averageDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
      case false => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.averageDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
    }
  }

  /** Assigns an object/hyper-edge to the closest cluster, according to the maximal distance to all elements in a cluster
    *
    * @param nt a list of neighbourhood trees representing an object/hyper-edge
    * @return the closest (most similar) cluster
    * */
  def assignToClosestClusterMaximal(nt: List[NeighbourhoodGraph]) = {
    nt.length == 1 match {
      case true => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.maximalDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
      case false => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.maximalDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
    }
  }

  /** Assigns an object/hyper-edge to the closest cluster, according to the minimal distance to all elements in a cluster
    *
    * @param nt a list of neighbourhood trees representing an object/hyper-edge
    * @return the closest (most similar) cluster
    * */
  def assignToClosestClusterMinimal(nt: List[NeighbourhoodGraph]) = {
    nt.length == 1 match {
      case true => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.minimalDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
      case false => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.minimalDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
    }
  }

}
