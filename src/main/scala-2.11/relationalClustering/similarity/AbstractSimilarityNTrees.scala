package relationalClustering.similarity

import java.io.File

import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 04.02.16.
  */
abstract class AbstractSimilarityNTrees(override protected val knowledgeBase: KnowledgeBase,
                                        override protected val depth: Int,
                                        protected val useLocalRepo: Boolean = false) extends AbstractSimilarityMeasure(knowledgeBase, depth) {

  /** caches neighbourhood graphs to be built only once
    * key (objectName, domain)
    * */
  protected val neighbourhoodGraphCache = collection.mutable.Map[(String,String), NeighbourhoodGraph]()

  /** Returns true if local Node Repository should be used for each [[NeighbourhoodGraph]] */
  protected def useLocal = {
    useLocalRepo
  }

  protected def getNeighbourhoodGraph(objectName: String, domain: String) = {
    val keytoUse = (objectName, domain)
    if (!neighbourhoodGraphCache.contains(keytoUse)) {
      neighbourhoodGraphCache(keytoUse) = useLocal match {
        case true => new NeighbourhoodGraph(objectName, domain, getDepth, getKB)
        case false => new NeighbourhoodGraph(objectName, domain, getDepth, getKB, getRepo)
      }
    }
    neighbourhoodGraphCache(keytoUse)
  }

  /** Creates all neighbourhood graphs (needed when similarities are read from file)*/
  def buildAllNeighbourhoodGraphs(domains: List[String]) = {
    if (neighbourhoodGraphCache.isEmpty) {
      getObjectsFromDomains(domains).foreach( t => {
        getNeighbourhoodGraph(t._1, t._2)
      })
    }
  }

  /** Returns the set of constructed neighbourhood graphs */
  def getNeighbourhoodGraphCache = {
    neighbourhoodGraphCache.toMap
  }

  /** Clears the neighbourhood graph cache*/
  def clearCache() = {
    neighbourhoodGraphCache.clear()
  }

  /** Get the similarity matrix and saves it to the file
    *
    * @param domains domains of interest
    * @param folder folder to save file
    * @return filename of the file containing similarity matrix, and the element ordering
    *
    * */
  override def getObjectSimilaritySave(domains: List[String], folder: String) = {
    if (!new File(s"$folder/${getFilename(domains)}").exists()) {
      val (elems, sim) = getObjectSimilarity(domains)
      saveMatrixToFile(s"$folder/${getFilename(domains)}", domains, elems, sim)
    }
    else {
      buildAllNeighbourhoodGraphs(domains)
    }

    val absolutePath = new File(s"$folder/${getFilename(domains)}")

    (absolutePath.getAbsolutePath, getObjectsFromDomains(domains))
  }

  /** Get the hyper-edge similarity matrix and save it to file
    *
    * @param domains domains of the hyperedges
    * @param folder folder to save the file to
    * @return fileName, an ordering of elements
    * */
  override def getHyperEdgeSimilaritySave(domains: List[String], folder: String) = {
    if (!new File(s"$folder/${getFilenameHyperEdges(domains)}").exists()) {
      val (elems, sim) = getHyperEdgeSimilarity(domains)
      saveMatrixToFile(s"$folder/${getFilenameHyperEdges(domains)}", domains, elems.map( _.mkString(":")), sim)
    }
    else {
      buildAllNeighbourhoodGraphs(domains)
    }

    val absolutePath = new File(s"$folder/${getFilenameHyperEdges(domains)}")

    //(s"$folder/${getFilenameHyperEdges(domains)}", getHyperEdges(domains))
    (absolutePath.getAbsolutePath,getHyperEdges(domains) )
  }

}
