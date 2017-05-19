package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagComparison.AbstractBagComparison
import relationalClustering.neighbourhood.{NeighbourhoodTree, Node}
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 18.03.16.
  */
class HSAG(override protected val knowledgeBase: KnowledgeBase,
           override protected val depth: Int,
           override protected val bagCompare: AbstractBagComparison) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.5,0.5,0.0,0.0,0.0), bagCompare, null, List[AbstractAggregator](), true) {

  override def getFilename(domains: List[String]): String = {
    s"${domains.mkString(",")}_hsag_localRepo$useLocal.txt"
  }

  /** Override to make sure the exception is thrown*/
  override def getHyperEdgeSimilarity(domains: List[String]): (List[List[String]], DenseMatrix[Double]) = {
    new Exception(s"HSAG similarity metrics cannot cluster hyperedges!!!")

    val hyperEdges = getHyperEdges(domains)
    val returnMat = DenseMatrix.zeros[Double](hyperEdges.length, hyperEdges.length)

    (hyperEdges, returnMat)
  }

  protected def contentSimilarity(ng1: Node, ng2: Node): Double = {
    val value = bagCompare.compareBags(ng1.getAttributeValuePairs.map(el => (el, 1)).toMap, ng2.getAttributeValuePairs.map(el => (el, 1)).toMap)
    value
  }

  protected def neighbourSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    val value = ng2.getRoot.getChildNodes.foldLeft(0.0)( (acc, node) => {
      acc + contentSimilarity(ng1.getRoot, node)
    })/ng2.getRoot.getChildNodes.length

    if (value.isNaN) {
      1.0
    } else {
      value
    }
  }

  protected def contextSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    (neighbourSimilarity(ng1, ng2) + neighbourSimilarity(ng2, ng1))/2.0
  }

  override protected def attributeSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    contentSimilarity(ng1.getRoot, ng2.getRoot)
  }

  override protected def attributeNeighbourhoodSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree): Double = {
    contextSimilarity(ng1, ng2)
  }

  override def getObjectSimilarity(domains: List[String], objectsToUse: List[(String, String)] = null): (List[String], DenseMatrix[Double]) = {
    val objects = objectsToUse == null match {
      case false => objectsToUse
      case true => getObjectsFromDomains(domains)
    }

    val functionsWithNorm = List(bagCompare.needsToBeInverted, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodTree, NeighbourhoodTree) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
    )

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](objects.length, objects.length))( (acc, w) => {
      acc + (DenseMatrix.tabulate(objects.length, objects.length) { (x, y) => w._1 } :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1, 0))
    })

    (objects.map(_._1), returnMat)
  }

}
