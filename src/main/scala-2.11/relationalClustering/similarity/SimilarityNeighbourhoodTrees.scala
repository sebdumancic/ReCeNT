package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.bagComparison.AbstractBagCompare
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.KnowledgeBase

/**
  * Implements the similarity measure using neighbourhood trees
  *
  * @constructor constructs a measure object with provided knowledge base, depth, weights and bag comparison method
  * @param knowledgeBase knowledge base: [[KnowledgeBase]]
  * @param depth depth of the neighbourhood graphs: [[Int]]
  * @param weights a list of weights (attributes, attribute neighbourhood, connections, vertex neighbourhoods, edge distribution), must sum to 1.0: [[List]]
  * @param bagCompare bag comparison class: [[AbstractBagCompare]]
  * Created by seb on 03.02.16.
  */
class SimilarityNeighbourhoodTrees(override protected val knowledgeBase: KnowledgeBase,
                                   override protected val depth: Int,
                                   protected val weights: List[Double],
                                   protected val bagCompare: AbstractBagCompare) extends AbstractSimilarityNTrees(knowledgeBase, depth) {

  require(weights.length == 5, s" You should provide 5 weights not ${weights.length} ($weights)")
  require(weights.sum == 1.0, s"Weights should sum to one, not ${weights.sum}")

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]) = {
    s"domains_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}.txt"
  }

  /***********************
    *
    *    OBJECT SIMILARITY
    * * * * * * */

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  def getObjectSimilarity(domains: List[String]) = {
    val objects = getObjectsFromDomains(domains)

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(NeighbourhoodGraph, NeighbourhoodGraph) => Double](attributeSimilarity, attributeNeighbourhoodSimilarity, elementConnections, vertexIdentityDistribution, edgeDistributionsSimilarity)
    )

    val returnMat = weights.zipWithIndex.filter( _._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](objects.length, objects.length))( (acc, w) => {
      acc + DenseMatrix.tabulate(objects.length, objects.length){case x => w._1} :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1)
    })

    (objects.map(_._1), returnMat)
  }

  /** Compute the attribute-value pair similarity between two root elements
    *
    * @param ng1 neighbourhood graph of the first element: [[NeighbourhoodGraph]]
    * @param ng2 neighbourhood graph of the second element: [[NeighbourhoodGraph]]
    * @return Double
    * */
  protected def attributeSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val attrs1 = ng1.getRootAttributes
    val attrs2 = ng2.getRootAttributes

    attrs1.intersect(attrs2).size
  }

  /** Computes the attribute neighbourhood similarity of two neighbourhood graphs, per level and vertex type
    *
    * @param ng1 the first elements's neighbourhood graph: [[NeighbourhoodGraph]]
    * @param ng2 the second elements's neighbourhood graph: [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def attributeNeighbourhoodSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val firstAttrs = ng1.getAttributeValueDistribution
    val secondAttrs = ng2.getAttributeValueDistribution

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + firstAttrs(depth).keySet.union(secondAttrs(depth).keySet).foldLeft(0.0)( (acc_i, vType) => {
        acc_i + math.abs(bagCompare.compareBags(firstAttrs(depth).getOrElse(vType, List[(String,String)]()).map( x => s"${x._1}_${x._2}"),
                                       secondAttrs(depth).getOrElse(vType, List[(String,String)]()).map( x => s"${x._1}_${x._2}")))
      })
    })
  }

  /** Calculates the number of connections between two root elements
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def elementConnections(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val depthOneVertices = ng2.collectVertexIdentity()(0).getOrElse(ng1.getRootDomain, List[String]())

    depthOneVertices.count( _ == ng1.getRoot.getEntity ).toDouble
  }

  /** Computes the vertex identity distributions similarity for two neighbourhood graphs, per level and vertex type
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def vertexIdentityDistribution(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val neighbourhood1 = ng1.collectVertexIdentity()
    val neighbourhood2 = ng2.collectVertexIdentity()

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + neighbourhood1(depth).keySet.union(neighbourhood2(depth).keySet).foldLeft(0.0)( (acc_i, vType) => {
        acc_i + math.abs(bagCompare.compareBags(neighbourhood1(depth).getOrElse(vType, List[String]()), neighbourhood2(depth).getOrElse(vType, List[String]())))
      })
    })
  }

  /** Calculates the edge distribution similarity between two neighbourhood graphs, per level
    *
    * @param ng1 the first element's [[NeighbourhoodGraph]]
    * @param ng2 the second element's [[NeighbourhoodGraph]]
    * @return [[Double]]
    * */
  protected def edgeDistributionsSimilarity(ng1: NeighbourhoodGraph, ng2: NeighbourhoodGraph) = {
    val distr1 = ng1.getEdgeDistribution
    val distr2 = ng2.getEdgeDistribution

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      acc + math.abs(bagCompare.compareBags(distr1(depth), distr2(depth)))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements list of element-domain tuples: [[List]]
    * @param simFunc similarity functions that takes two neighbourhood trees as input and return a Double
    * @param shouldBeInverted flag indicating should the computed matrix be inverted in order to be a similarity measure: [[Boolean]]
    * */
  protected def accumulateIntoMatrix(elements: List[(String,String)], simFunc: (NeighbourhoodGraph, NeighbourhoodGraph) => Double, shouldBeInverted: Boolean) = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunc(getNeighbourhoodGraph(elements(ind1)._1, elements(ind1)._2), getNeighbourhoodGraph(elements(ind2)._1, elements(ind2)._2))

      similarityMatrix(ind1, ind2) = simValue
      similarityMatrix(ind2, ind1) = simValue
    }

    shouldBeInverted match {
      case true => normalizeAndInvert(similarityMatrix)
      case false => normalizeMatrix(similarityMatrix)
    }
  }




  /*********************************
    *
    *   HYPEREDGE SIMILARITY
    *
    */

  /** Method implementing an interface to assess the similarity of edges connecting objects from specified domains
    *
    * @param domains list of domains that hyperedges connect: [[List]]
    * @return (ordering of hyperEdges, similarity matrix)
    * */
  def getHyperEdgeSimilarity(domains: List[String]) = {
    new NotImplementedError("HyperEdge similarity not implemented yet!")
    (List[List[String]](), DenseMatrix.zeros[Double](1,1))
  }
}
