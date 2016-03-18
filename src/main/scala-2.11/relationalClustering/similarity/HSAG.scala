package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 18.03.16.
  */
class HSAG(override protected val knowledgeBase: KnowledgeBase,
           override protected val depth: Int,
           override protected val bagCompare: AbstractBagComparison) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.5,0.5,0.0,0.0,0.0), bagCompare, null, true) {

  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_hsag_localRepo$useLocal.txt"
  }

  /** Override to make sure the exception is thrown*/
  override def getHyperEdgeSimilarity(domains: List[String]) = {
    new Exception(s"HSAG similarity metrics cannot cluster hyperedges!!!")

    val hyperEdges = getHyperEdges(domains)
    val returnMat = DenseMatrix.zeros[Double](hyperEdges.length, hyperEdges.length)

    (hyperEdges, returnMat)
  }

}
