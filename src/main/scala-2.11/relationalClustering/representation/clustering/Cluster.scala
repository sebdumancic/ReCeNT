package relationalClustering.representation.clustering

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.neighbourhood.NeighbourhoodGraph


/** Implements cluster functionality (container of instances)
  * Created by seb on 17.03.16.
  */
abstract class Cluster(protected val types: List[String],
                       protected val clusterName: String,
                       protected val instances: Set[List[String]],
                       protected val ntRepo: Map[(String,String), NeighbourhoodGraph]) {

  /** Returns the cluster type
    * */
  def getTypes = {
    types
  }

  /** Returns cluster name
    * */
  def getClusterName = {
    clusterName
  }

  def distance(nt: NeighbourhoodGraph, weights: List[Double], bagCompare: AbstractBagComparison, bagCombine: AbstractBagCombine): Double
}
