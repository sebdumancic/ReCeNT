package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.SimpleCountSimilarity
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.Settings

/**
  * Created by seb on 15.03.16.
  */
class NevilleSimilarityMeasure(override protected val knowledgeBase: KnowledgeBase) extends SimilarityNeighbourhoodTrees(knowledgeBase, 0, List(1.0,0.0,0.0,0.0,0.0), new SimpleCountSimilarity, new UnionCombination, List[AbstractAggregator]()) {

  /** Extracts all connections between elements of a certain type
    *
    * @param domain domain of the elements
    * @return set of 2-tuples containing connected elements
    * */
  def getConnectionBetweenElements(domain: String) = {
    val resultingTuples = collection.mutable.Set[(String,String)]()

    getKB.getPredicateNames.map( getKB.getPredicate).filter( _.getRole == Settings.ROLE_HYPEREDGE).filter( _.getDomains.count( d => d == domain) > 1).foreach( predicate => {
      val doms = predicate.getDomains.zipWithIndex.filter( _._1 == domain).map( _._2)
      predicate.getTrueGroundings.map( x => x.zipWithIndex.filter( elem => doms.contains(elem._2)).map( _._1)).foreach( ground => {
        ground.sorted.combinations(2).foreach(comb => {
          resultingTuples += new Tuple2[String,String](comb.head, comb.last)
        })
      })
    })

    resultingTuples.toSet
  }

  override def getObjectSimilarity(domains: List[String], objectsToUse: List[(String, String)] = null) = {
    require(domains.length == 1, s"Neville assumes all vertices are of the same type -> only one type can be given (currently $domains are given)")
    val objects = getObjectsFromDomains(domains)
    val noDomainObjects = objects.map(_._1)

    val functionsWithNorm = List(false).zip(List[(NeighbourhoodGraph, NeighbourhoodGraph) => Double](attributeSimilarity))

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](objects.length, objects.length))( (acc, w) => {
      acc + (DenseMatrix.tabulate(objects.length, objects.length) { (x, y) => w._1 } :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1, 0))
    })

    val linkMatrix = DenseMatrix.zeros[Double](objects.length, objects.length)
    getConnectionBetweenElements(domains.head).foreach( conn => {
      linkMatrix(noDomainObjects.indexOf(conn._1), noDomainObjects.indexOf(conn._2)) = 1.0
      linkMatrix(noDomainObjects.indexOf(conn._2), noDomainObjects.indexOf(conn._1)) = 1.0
    })

    (objects.map(_._1), returnMat :* linkMatrix)
  }

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  override def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_neville_localRepo$useLocal.txt"
  }



  /** Override to make sure the exception is thrown*/
  override def getHyperEdgeSimilarity(domains: List[String]) = {
    new Exception(s"Neville's similarity metrics cannot cluster hyperedges!!!")

    val hyperEdges = getHyperEdges(domains)
    val returnMat = DenseMatrix.zeros[Double](hyperEdges.length, hyperEdges.length)

    (hyperEdges, returnMat)
  }
}
