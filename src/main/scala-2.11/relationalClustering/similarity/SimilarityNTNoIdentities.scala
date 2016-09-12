package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 22.03.16.
  */
class SimilarityNTNoIdentities(override protected val knowledgeBase: KnowledgeBase,
                               override protected val depth: Int,
                               override protected val weights: List[Double],
                               override protected val bagCompare: AbstractBagComparison,
                               override protected val bagCombine: AbstractBagCombine,
                               override protected val aggregators: List[AbstractAggregator],
                               override protected val useLocalRepo: Boolean = false) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, weights, bagCompare, bagCombine, aggregators, useLocalRepo){


  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo${useLocal}_noidentities.txt"
  }

  override def getFilenameHyperEdges(domains: List[String]) = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_combination${bagCombine.getName}_localRepo${useLocal}_noidentities.txt"
  }

  /** Compares the distributions over vertex types, depth-wise
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return similarity [[Double]]
    * */
  protected def vertexTypeDistribution(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val neighbourhood1 = ng1.collectVertexIdentity()
    val neighbourhood2 = ng2.collectVertexIdentity()

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val flatN1 = neighbourhood1(depth).flatMap( vType => vType._2.map( x => vType._1))
      val flatN2 = neighbourhood2(depth).flatMap( vType => vType._2.map( x => vType._1))

      acc + math.abs(bagCompare.compareBags(flatN1.toList, flatN2.toList))
    })
  }

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  override def getObjectSimilarity(domains: List[String]) = {
    val objects = getObjectsFromDomains(domains)

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodGraph, NeighbourhoodGraph) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexTypeDistribution, edgeDistributionsSimilarity)
    )

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](objects.length, objects.length))( (acc, w) => {
      acc + (DenseMatrix.tabulate(objects.length, objects.length){case x => w._1} :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1, w._2))
    })

    (objects.map(_._1), returnMat)
  }


  // TODO: change the similarity computation in hyperedges part as well
}
