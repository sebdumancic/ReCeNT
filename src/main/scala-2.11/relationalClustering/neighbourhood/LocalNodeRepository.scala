package relationalClustering.neighbourhood

import relationalClustering.representation.KnowledgeBase

/**
  * Created by seb on 10.02.16.
  */
class LocalNodeRepository(override protected val knowledgeBase: KnowledgeBase) extends NodeRepository(knowledgeBase, true) {

  override def getNode(identity: String, domain: String) = {
    if (!createdNodes.contains(identity)) {
      val newNode = new LocalNode(identity, domain)
      addDescriptions(newNode)
      addToCache(newNode)
    }
    createdNodes(identity)
  }
}
