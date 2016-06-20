package relationalClustering.representation.clustering

import relationalClustering.neighbourhood.NeighbourhoodGraph


/** Implements cluster functionality (container of instances)
  * Created by seb on 17.03.16.
  */
class Cluster(protected val types: List[String],
              protected val clusterName: String,
              protected val instances: Set[List[String]],
              protected val ntRepo: Map[(String,String), NeighbourhoodGraph]) {

  /** Returns the cluster type */
  def getTypes = {
    types
  }

  /** Returns cluster name */
  def getClusterName = {
    clusterName
  }

  /** Returns instance names */
  def getInstances = {
    instances
  }

  /** Returns the definition of a predicate representing the cluster */
  def getDefinition = {
    s"$getClusterName(${getTypes.mkString(",")})"
  }

  /** Returns elements in cluster written as logical facts */
  def getClusterFacts = {
    getInstances.map(inst => s"$getClusterName(${inst.mkString(",")})")
  }

  /** Returns declaration of the predicate associated with the cluster */
  def getClusterDeclaration = {
    s"$getClusterName(${getTypes.map( t => "name").mkString(",")})"
  }

  /** Returns a set of neighbourhood trees for the given instance*/
  def getInstanceNeighbourhoodTree(instance: List[String]) = {
    instance.zip(types).map( it => ntRepo(it._1, it._2))
  }

  /** Returns the number of elements in the cluster*/
  def getSize = {
    instances.size
  }
}
