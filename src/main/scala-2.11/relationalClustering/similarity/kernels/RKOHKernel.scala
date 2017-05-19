package relationalClustering.similarity.kernels

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagComparison.ChiSquaredDistance
import relationalClustering.neighbourhood.{Edge, NeighbourhoodTree}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Implements the kernel from "Learning from Interpretations: A Rooted Kernel for Ordered Hypergraphs" by Wachman and Khardon (ICML 2007)
  * Created by seb on 06.04.16.
  */
class RKOHKernel(override protected val knowledgeBase: KnowledgeBase,
                 override protected val depth: Int,
                 protected val walkLength: Int) extends SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(1.0, 0.0, 0.0, 0.0, 0.0), new ChiSquaredDistance(), null, List[AbstractAggregator](), true) {

  protected def getWalkLength = {
    walkLength
  }

  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_walklength${getWalkLength}_rkoh.txt"
  }

  /** Calculates the K step defined at the paper
    *
    * @param p1 an edge from the first graph
    * @param p2 an edge from the second graph
    * @param iteration iteration of the step
    * */
  def Kstep(p1: Edge, p2: Edge, iteration: Int): Int = {
    if (iteration <= 1) {
        // base case
        p1.getPredicate.getName == p2.getPredicate.getName match {
          case true => 1
          case false => 0
        }
    }
    else {
      Kstep(p1, p2, 1) == 0 match {
        case true => 0
        case false =>
          // arity loop
          (0 until p1.arity).foldLeft(0)( (acc, i) => {
            // max arity loop
            acc + (0 until 2).foldLeft(0)( (acc_a, j) => {
              // unary edges
              val p1UnaryEdges = i == 0 match {
                case true => p1.getParent.getAnnotationsAsUnaryEdges(getKB)
                case false => p1.getChild.getAnnotationsAsUnaryEdges(getKB)
              }
              val p2UnaryEdges = i == 0 match {
                case true => p2.getParent.getAnnotationsAsUnaryEdges(getKB)
                case false => p2.getChild.getAnnotationsAsUnaryEdges(getKB)
              }

              //remaining edges
              val p1Edges = (p1.getVertices(i).getChildEdges ++ p1.getVertices(i).getParentEdges).filter( ed => ed.arity >= j).filter(ed => ed.getVertices(j).getEntity == p1.getVertices(i).getEntity)
              val p2Edges = (p2.getVertices(i).getChildEdges ++ p2.getVertices(i).getParentEdges).filter( ed => ed.arity >= j).filter(ed => ed.getVertices(j).getEntity == p2.getVertices(i).getEntity)

              acc_a + (p1Edges ++ p1UnaryEdges).foldLeft(0)( (acc_i, ed1) => {
                acc_i + (p2Edges ++ p2UnaryEdges).foldLeft(0)( (acc_ii, ed2) => {
                  acc_ii + Kstep(ed1, ed2, iteration - 1)
                })
              })
            })
          })
      }
    }
  }

  override protected def attributeSimilarity(ng1: NeighbourhoodTree, ng2: NeighbourhoodTree) = {
    val attrs1 = ng1.getAllEdges(true)
    val attrs2 = ng2.getAllEdges(true)

    attrs1.foldLeft(0)( (acc, ed1) => {
      acc + attrs2.foldLeft(0)( (acc_i, ed2) => {
        val kval = Kstep(ed1,ed2, getWalkLength)
        acc_i + kval
      })
    }).toDouble
  }
}
