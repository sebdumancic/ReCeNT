package relationalClustering.representation.clustering

import relationalClustering.neighbourhood.NeighbourhoodTree
import relationalClustering.similarity.AbstractSimilarityNTrees


/** Implements cluster functionality (container of instances)
  * Created by seb on 17.03.16.
  */
class Cluster(protected val types: List[String],
              protected val clusterName: String,
              protected val instances: Set[List[String]],
              protected val ntRepo: Map[(String, String), NeighbourhoodTree]) {

  /** Returns the cluster type */
  def getTypes: List[String] = {
    types
  }

  /** Returns cluster name */
  def getClusterName: String = {
    clusterName
  }

  /** Returns instance names */
  def getInstances: Set[List[String]] = {
    instances
  }

  def getRepo: Map[(String, String), NeighbourhoodTree] = {
    ntRepo
  }

  /** Returns the definition of a predicate representing the cluster */
  def getDefinition: String = {
    s"$getClusterName(${getTypes.mkString(",")})"
  }

  /** Returns elements in cluster written as logical facts */
  def getClusterFacts: Set[String] = {
    getInstances.map(inst => s"$getClusterName(${inst.mkString(",")})")
  }

  /** Returns declaration of the predicate associated with the cluster */
  def getClusterDeclaration: String = {
    s"$getClusterName(${getTypes.map( t => "name").mkString(",")})"
  }

  /** Returns a set of neighbourhood trees for the given instance*/
  def getInstanceNeighbourhoodTree(instance: List[String]): List[NeighbourhoodTree] = {
    instance.zip(types).map( it => ntRepo(it._1, it._2))
  }

  /** Returns the number of elements in the cluster*/
  def getSize: Int = {
    instances.size
  }

  def similarityToCluster(nt: List[NeighbourhoodTree], similarity: AbstractSimilarityNTrees, flag: String = "maximal"): Double = {
    val similarities = nt.length == 1 match {
      case true => getInstances.map(inst => similarity.pairObjectSimilarity(nt.head, getInstanceNeighbourhoodTree(inst).head))
      case false => getInstances.map(inst => similarity.getPairHyperEdgeSimilarity(nt, getInstanceNeighbourhoodTree(inst)))
    }

    flag match {
      case "maximal" => similarities.max
      case "average" => similarities.sum/similarities.size
    }
  }
}
