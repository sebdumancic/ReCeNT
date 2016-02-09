package relationalClustering.similarity

import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.KnowledgeBase

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
    val keytoUse = new Tuple2(objectName, domain)
    if (!neighbourhoodGraphCache.contains(keytoUse)) {
      neighbourhoodGraphCache(keytoUse) = useLocal match {
        case true => new NeighbourhoodGraph(objectName, domain, getDepth, getKB)
        case false => new NeighbourhoodGraph(objectName, domain, getDepth, getKB, getRepo)
      }
    }
    neighbourhoodGraphCache(keytoUse)
  }

}
