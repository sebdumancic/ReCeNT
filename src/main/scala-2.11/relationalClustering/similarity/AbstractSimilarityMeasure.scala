package relationalClustering.similarity

import breeze.linalg.DenseMatrix
import relationalClustering.neighbourhood.NodeRepository
import relationalClustering.representation.KnowledgeBase

/** Abstract Similarity measure class
  * Created by seb on 18.01.16.
  */
abstract class AbstractSimilarityMeasure(protected val knowledgeBase: KnowledgeBase,
                                         protected val depth: Int) {

  protected val nodeRepository = new NodeRepository(knowledgeBase)

  /** Returns the specified depth */
  def getDepth = {
    depth
  }

  /** Returns the knowledge base */
  protected def getKB = {
    knowledgeBase
  }

  /** Returns the node repository */
  protected def getRepo = {
    nodeRepository
  }

  /** Composes a list of object in specified domains
    *
    * @param domains list of domains to compose objects from
    * @return list of (object name, domain)
    * */
  protected def getObjectsFromDomains(domains: List[String]) = {
    domains.foldLeft(List[(String,String)]())( (acc, dom) => {
      acc ++ getKB.getDomain(dom).getElements.map( x => new Tuple2(x, dom))
    })
  }

  /** Method implementing an interface for accessing similarity of object(s) from specified domain(s)
    *
    * @param domains list of domains to cluster objects from: [[List]]
    * @return (ordering of objects, similarity matrix for corresponding element)
    * */
  protected def getObjectSimilarity(domains: List[String]): (List[String], DenseMatrix[Double])

  /** Uniquely identifies the filename to save similarity matrix (once calculated it can be reused)
    *
    * @param domains list of domains of interest
    * */
  def getFilename(domains: List[String]): String

  /** Method implementing an interface to assess the similarity of edges connecting objects from specified domains
    *
    * @param domains list of domains that hyperedges connect: [[List]]
    * @return (ordering of hyperEdges, similarity matrix)
    * */
  protected def getHyperEdgeSimilarity(domains: List[String]): (List[List[String]], DenseMatrix[Double])


}
