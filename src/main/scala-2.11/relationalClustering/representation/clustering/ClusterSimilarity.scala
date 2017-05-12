package relationalClustering.representation.clustering

import relationalClustering.neighbourhood.NeighbourhoodTree
import relationalClustering.similarity.AbstractSimilarityNTrees

/**
  * Created by seb on 07.06.16.
  */
object ClusterSimilarity {

  /** OBJECT/VERTEX DISTANCES
    * */


  /** Returns the average distance of an instance to all instances in the cluster
    *
    * @param instanceNT neighbourhood tree of an instance
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def averageSimilarityObject(instanceNT: NeighbourhoodTree, cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == 1)
    cluster.getInstances.map(obj => similarity.pairObjectSimilarity(instanceNT, cluster.getInstanceNeighbourhoodTree(obj).head)).sum/cluster.getInstances.size
  }

  /** Returns the maximum distance of an instance to all instances in the cluster
    *
    * @param instanceNT neighbourhood tree of an instance
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def maximalSimilarityObject(instanceNT: NeighbourhoodTree, cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == 1)
    cluster.getInstances.map(obj => similarity.pairObjectSimilarity(instanceNT, cluster.getInstanceNeighbourhoodTree(obj).head)).max
  }


  /** Returns the minimum distance of an instance to all instances in the cluster
    *
    * @param instanceNT neighbourhood tree of an instance
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def minimalSimilarityObject(instanceNT: NeighbourhoodTree, cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == 1)
    cluster.getInstances.map(obj => similarity.pairObjectSimilarity(instanceNT, cluster.getInstanceNeighbourhoodTree(obj).head)).min
  }



  /** RELATION/HYPER-EDGE DISTANCES
    * */

  /** Returns the average distance of an instance (hyper-edge) to all instances in the cluster
    *
    * @param instance a list of neighbourhood trees representing a hyper-edge
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def averageSimilarityEdge(instance: List[NeighbourhoodTree], cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == instance.length)
    cluster.getInstances.map(edge => similarity.getPairHyperEdgeSimilarity(instance, cluster.getInstanceNeighbourhoodTree(edge))).sum/cluster.getInstances.size
  }

  /** Returns the maximal distance of an instance (hyper-edge) to all instances in the cluster
    *
    * @param instance a list of neighbourhood trees representing a hyper-edge
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def maximalSimilarityEdge(instance: List[NeighbourhoodTree], cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == instance.length)
    cluster.getInstances.map(edge => similarity.getPairHyperEdgeSimilarity(instance, cluster.getInstanceNeighbourhoodTree(edge))).max
  }

  /** Returns the minimal distance of an instance (hyper-edge) to all instances in the cluster
    *
    * @param instance a list of neighbourhood trees representing a hyper-edge
    * @param cluster cluster of relational objects
    * @param similarity similarity measure for comparison (all the parameters and norm constants have to be calculated)
    * @return similarity value
    * */
  def minimalSimilarityEdge(instance: List[NeighbourhoodTree], cluster: Cluster, similarity: AbstractSimilarityNTrees) = {
    require(cluster.getInstances.head.length == instance.length)
    cluster.getInstances.map(edge => similarity.getPairHyperEdgeSimilarity(instance, cluster.getInstanceNeighbourhoodTree(edge))).min
  }

}
