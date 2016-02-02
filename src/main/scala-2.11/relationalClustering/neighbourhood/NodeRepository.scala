package relationalClustering.neighbourhood

import relationalClustering.representation.KnowledgeBase

/**
  * Implements the functionality of the node repository so that all node are created only once
  *
  * @constructor constructs the repository for a certain knowledge base
  * @param knowledgeBase knowledge base for the Nodes
  * Created by seb on 02.02.16.
  */
class NodeRepository(protected val knowledgeBase: KnowledgeBase) {

  // Cache for created nodes: domain -> name of the object -> Node
  protected val createdNodes = collection.mutable.Map[String,Node]()

  /** Adds the node to the node cache
    *
    * @param node Node to be added: Node
    * */
  protected def addToCache(node: Node) = {
    createdNodes(node.getEntity)= node
  }

  /** Gets the node from the cache; if the node does not exist, it is created
    *
    * @param identity name of the node: String
    * @param domain domain of the node: String
    * @return Node
    * */
  def getNode(identity: String, domain: String) = {
    if (!createdNodes.contains(identity)) {
      addToCache(new Node(identity, domain))
    }
    createdNodes(identity)
  }

}
