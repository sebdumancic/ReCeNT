package relationalClustering.neighbourhood

import relationalClustering.representation.KnowledgeBase
import relationalClustering.utils.Settings

/**
  * Implements the functionality of the node repository so that all node are created only once
  *
  * @constructor constructs the repository for a certain knowledge base
  * @param knowledgeBase knowledge base for the Nodes
  *
  * Node repository takes care of the attributes and annotations associated with a node
  * Created by seb on 02.02.16.
  */
class NodeRepository(protected val knowledgeBase: KnowledgeBase) {

  // Cache for created nodes: name of the object -> Node
  protected val createdNodes = collection.mutable.Map[String,Node]()

  /** Returns a knowledge base */
  def getKB = {
    knowledgeBase
  }

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
      val newNode = new Node(identity, domain)
      addDescriptions(newNode)
      addToCache(newNode)
    }
    createdNodes(identity)
  }

  /** Adds attribute values to the node
    *
    * @param node node to be extended by attributes: [[Node]]
    *
    * Facts in the form of Predicate(Object,Value), where [Value] is declared as 'attribute' and [Object] as 'name', are considered attributes
    * */
  private def addAttributes(node: Node) = {
    getKB.getPredicateNames.map( getKB.getPredicate).filter( _.getRole == Settings.ROLE_ATTRIBUTE ).filter(_.getDomains.contains(node.getDomain)).foreach(predicate => {
      val attributePosition = predicate.getArgumentRoles.zipWithIndex.filter( _._1 == Settings.ARG_TYPE_ATTRIBUTE )
      predicate.getTrueGroundings.filter( _.contains(node.getEntity)).foreach( grounding => {
        attributePosition.foreach( pos => {
          node.addAttributeValue(predicate.getName, grounding(pos._2))
        })
      })
    })
  }

  /** Adds annotations to the node
    *
    * @param node node to be extended by annotations: [[Node]]
    *
    * Facts in form of Predicate(Object), where [Object] is declared as 'name', are considered annotations
    * */
  private def addAnnotations(node: Node) = {
    getKB.getPredicateNames.map( getKB.getPredicate).filter( _.getRole == Settings.ROLE_ANNOTATION).filter( _.getDomains.contains(node.getDomain)).foreach( ann => {
      if (ann.getTrueGroundings.contains(List[String](node.getEntity))) {
        node.addAnnotation(ann.getName)
      }
      else {
        node.addFalseAnnotation(ann.getName)
      }
    })
  }

  /** Adds annotations and attributes to the node
    *
    * @param node node to be extended by descriptions: [[Node]]
    * */
  private def addDescriptions(node: Node) = {
    addAnnotations(node)
    addAttributes(node)
  }

}
