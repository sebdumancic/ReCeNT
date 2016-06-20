package relationalClustering.representation.clustering

import java.io.{File, FileWriter}

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


  /** A generic method to assign a neighbourhood tree to the closest cluster
    *
    * @param nt a list of neighbourhood trees representing an object/hyper-edge
    * @param linkage linkage for the similarity calculation [average|maximal|minimal]
    * @return the most similar cluster
    * */
  def assignToClosestCluster(nt: List[NeighbourhoodGraph], linkage: String = "average") = {
    nt.length == 1 match {
      case true =>
        linkage match {
          case "average" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.averageDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
          case "maximal" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.maximalDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
          case "minimal" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.minimalDistanceObject(nt.head, clust._1, similarityMeasure))).maxBy(_._2)
        }
      case false =>
        linkage match {
          case "average" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.averageDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
          case "maximal" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.maximalDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
          case "minimal" => getClusters.zipWithIndex.map(clust => (clust._1, ClusterDistance.minimalDistanceEdge(nt, clust._1, similarityMeasure))).maxBy(_._2)
        }
    }
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

  /** Prints clustering to a file
    *
    * @param filePointer object representing a file to write in
    * */
  def printClusteringAsFacts(filePointer: FileWriter) = {
    clusters.foreach(clust => {
      filePointer.write(clust.getClusterFacts.mkString(File.separator) + File.separator)
    })
    filePointer.write(File.separator)
  }

  /** Prints definition of the predicates associated with the clustering
    *
    * @param filePointer object representing a file to write in
    * */
  def printClusteringDefinition(filePointer: FileWriter) = {
    clusters.foreach( clust => {
      filePointer.write(clust.getDefinition + File.separator)
    })
    filePointer.write(File.separator)
  }

  /** Prints a declaration of the predicates associated with the clustering
    *
    * @param filePointer object representing a file to write in
    * */
  def printClusteringDeclaration(filePointer: FileWriter) = {
    clusters.foreach(clust => {
      filePointer.write(clust.getClusterDeclaration + File.separator)
    })
    filePointer.write(File.separator)
  }

}
