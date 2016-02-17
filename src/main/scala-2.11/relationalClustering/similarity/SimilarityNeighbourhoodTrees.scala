package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.KnowledgeBase

/**
  * Implements the similarity measure using neighbourhood trees
  *
  * @constructor constructs a measure object with provided knowledge base, depth, weights and bag comparison method
  * @param knowledgeBase knowledge base: [[KnowledgeBase]]
  * @param depth depth of the neighbourhood graphs: [[Int]]
  * @param weights a list of weights (attributes, attribute neighbourhood, connections, vertex neighbourhoods, edge distribution), must sum to 1.0: [[List]]
  * @param bagCompare bag comparison class: [[AbstractBagComparison]]
  * Created by seb on 03.02.16.
  */
class SimilarityNeighbourhoodTrees(override protected val knowledgeBase: KnowledgeBase,
                                   override protected val depth: Int,
                                   protected val weights: List[Double],
                                   protected val bagCompare: AbstractBagComparison,
                                   protected val bagCombine: AbstractBagCombine,
                                   override protected val useLocalRepo: Boolean = false) extends AbstractSimilarityNTrees(knowledgeBase, depth, useLocalRepo) {

  require(weights.length == 5, s" You should provide 5 weights not ${weights.length} ($weights)")
  require(weights.sum == 1.0, s"Weights should sum to one, not ${weights.sum}")

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]) = {
    s"${domains.mkString(",")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.txt"
  }

  def getFilenameHyperEdges(domains: List[String]) = {
    s"${domains.mkString("")}_depth${depth}_parameters${weights.mkString(",")}_compare${bagCompare.name}_localRepo$useLocal.txt"
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
      acc + (DenseMatrix.tabulate(objects.length, objects.length){case x => w._1} :* accumulateIntoMatrix(objects, functionsWithNorm(w._2)._2, functionsWithNorm(w._2)._1))
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
        acc_i + math.abs(bagCompare.compareBags(firstAttrs(depth).getOrElse(vType, List[(String,String)]()),
                                                secondAttrs(depth).getOrElse(vType, List[(String,String)]())))
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

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
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
    val hyperEdges = getHyperEdges(domains)

    val functionsWithNorm = List(false, bagCompare.needsToBeInverted, false, bagCompare.needsToBeInverted, bagCompare.needsToBeInverted).zip(
      List[(List[NeighbourhoodGraph], List[NeighbourhoodGraph]) => Double](hyperedgeAttributeSimilarity, hyperEdgeAttributeNeighbourhoodSimilarity, hyperEdgeConnections, hyperEdgeVertexDistribution, hyperEdgeEdgeDistribution)
    )

    val returnMat = weights.zipWithIndex.filter(_._1 > 0.0).foldLeft(DenseMatrix.zeros[Double](hyperEdges.length, hyperEdges.length))( (acc, weight) => {
      acc + (accumulateIntoMatrixHyperEdge(hyperEdges, domains, functionsWithNorm(weight._2)._2, functionsWithNorm(weight._2)._1) :* DenseMatrix.tabulate(hyperEdges.length, hyperEdges.length){case x => weight._1})
    })

    (hyperEdges, returnMat)
  }

  /** computes the similarity between hyperEdge in vertices according to their attributes
    *
    * @param ngs1 the first hyperedge (ordered set of vertices)
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperedgeAttributeSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val ngOneCombined = ngs1.map(_.getRootAttributes).foldLeft(Set[(String,String)]())((acc, attrSet) => {
      bagCombine.combineBags(acc.toList, attrSet.toList).toSet
    })
    val ngTwoCombined = ngs2.map(_.getRootAttributes).foldLeft(Set[(String,String)]())( (acc, attrSet) => {
      bagCombine.combineBags(acc.toList, attrSet.toList).toSet
    })

    ngOneCombined.intersect(ngTwoCombined).size.toDouble
  }

  /** Computes the similarity between hyperEdges accoring to the attribute neighbourhoods of the vertices it connects
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeAttributeNeighbourhoodSimilarity(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstNeighbourhoods = ngs1.map(_.getAttributeValueDistribution)
    val secondNeighbourhoods = ngs2.map(_.getAttributeValueDistribution)

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFilteredAttrs = firstNeighbourhoods.map( _(depth))
      val secondDepthFilteredAttrs = secondNeighbourhoods.map(_(depth))
      val keys = firstDepthFilteredAttrs.map( _.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet).union(
        secondDepthFilteredAttrs.map( _.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet)
      )
      acc + keys.foldLeft(0.0)( (acc_i, vType) => {
        val firstBag = firstDepthFilteredAttrs.map( _.getOrElse(vType, List[(String,String)]())).foldLeft(List[(String,String)]())((acc, attrSet) => {
          bagCombine.combineBags(acc, attrSet)
        })
        val secondBag = secondDepthFilteredAttrs.map(_.getOrElse(vType, List[(String,String)]())).foldLeft(List[(String,String)]())( (acc, attrSet) => {
          bagCombine.combineBags(acc, attrSet)
        })
        acc_i + math.abs(bagCompare.compareBags(firstBag.map(e => s"${e._1}${e._2}"), secondBag.map(e => s"${e._1}${e._2}")))
      })
    })
  }

  /** Calculates the number of connections between vertices in two hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeConnections(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val depthOneVertices = ngs2.map( _.collectVertexIdentity()(0)).map( x => x.foldLeft(List[String]())( (acc, dom) => {
      acc ++ dom._2
    })).reduce(_ ++ _)

    ngs1.map( _.getRoot.getEntity).foldLeft(0.0)( (acc, elem) => {
      acc + depthOneVertices.count( _ == elem).toDouble
    })
  }

  /** Calculates the similarity in vertex neighbourhoods of the vertices in hyperedges
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeVertexDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstVertices = ngs1.map( _.collectVertexIdentity())
    val secondVertices = ngs2.map(_.collectVertexIdentity())

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFiltered = firstVertices.map( _(depth))
      val secondDepthFiltered = secondVertices.map(_(depth))
      val iterables = firstDepthFiltered.map(_.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet).union(
        secondDepthFiltered.map(_.keySet.toList).foldLeft(Set[String]())( (acc, dom) => bagCombine.combineBags(acc.toList, dom).toSet)
      )
      acc + iterables.foldLeft(0.0)( (acc_i, vType) => {
        val firstBag = firstDepthFiltered.map(_.getOrElse(vType, List[String]())).foldLeft(List[String]())( (acc, vSet) => bagCombine.combineBags(acc, vSet))
        val secondBag = secondDepthFiltered.map(_.getOrElse(vType, List[String]())).foldLeft(List[String]())( (acc, vSet) => bagCombine.combineBags(acc, vSet))
        acc_i + math.abs(bagCompare.compareBags(firstBag, secondBag))
      })
    })
  }

  /** Calculates the edge distribution similarity between vertices in a hyperedge
    *
    * @param ngs1 the first hyperedge
    * @param ngs2 the second hyperedge
    * @return [[Double]]
    * */
  protected def hyperEdgeEdgeDistribution(ngs1: List[NeighbourhoodGraph], ngs2: List[NeighbourhoodGraph]) = {
    val firstDistribution = ngs1.map(_.getEdgeDistribution)
    val secondDistribution = ngs2.map(_.getEdgeDistribution)

    (0 to getDepth).foldLeft(0.0)( (acc, depth) => {
      val firstDepthFiltered = firstDistribution.map( _(depth))
      val secondDepthfiltered = secondDistribution.map(_(depth))

      acc + math.abs(bagCompare.compareBags(firstDepthFiltered.foldLeft(List[String]())( (acc, eSet) => bagCombine.combineBags(acc, eSet)),
                                            secondDepthfiltered.foldLeft(List[String]())( (acc, eSet) => bagCombine.combineBags(acc, eSet))))
    })
  }

  /** Accumulates the similarity measure of provided elements in a matrix
    *
    * @param elements a list of hyperedges
    * @param domains domains of elements in a hyperedge
    * @param simFunction similarity measure
    * */
  protected def accumulateIntoMatrixHyperEdge(elements: List[List[String]], domains: List[String], simFunction: (List[NeighbourhoodGraph], List[NeighbourhoodGraph]) => Double, shouldBeInverted: Boolean) = {
    val similarityMatrix = DenseMatrix.zeros[Double](elements.length, elements.length)

    for(ind1 <- elements.indices; ind2 <- (ind1 + 1) until elements.length) {
      val simValue = simFunction(elements(ind1).zipWithIndex.map(x => getNeighbourhoodGraph(x._1, domains(x._2))),
                                 elements(ind2).zipWithIndex.map( x => getNeighbourhoodGraph(x._1, domains(x._2))))

      similarityMatrix(ind1, ind2) += simValue
      similarityMatrix(ind2, ind1) += simValue
    }

    shouldBeInverted match {
      case true => normalizeAndInvert(similarityMatrix)
      case false => normalizeMatrix(similarityMatrix)
    }
  }
}
