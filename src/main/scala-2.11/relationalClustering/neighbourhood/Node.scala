package relationalClustering.neighbourhood

import relationalClustering.representation.Predicate

/** Implements the node functionality in the [[relationalClustering.neighbourhood.NeighbourhoodGraph]]
  *
  * @constructor Create a new node for an entity of a domain.
  * @param entity the name of the root elements [String]
  * @param domain the domain of the root element [String]
  *
  *
  *
  * Created by seb on 01.02.16.
  */
class Node(protected val entity: String,
           protected val domain: String) {

  protected var parents = Set[Edge]()
  protected var children = Set[Edge]()
  protected val attributes = collection.mutable.Map[String, String]()
  protected val annotations = collection.mutable.Set[String]()

  /** Returns the domain of the root element */
  def getDomain = {
    domain
  }

  /** Returns the name of the root element */
  def getEntity = {
    entity
  }

  /** Adds an attribute value to the node
    *
    * @param attr name of the attribute: String
    * @param value value of the parameter: String
    *
    * */
  def addAttributeValue(attr: String, value: String) = {
    attributes(attr) = value
  }

  /** Retrieves the attribute value
    *
    * @param attr name of the attribute: String
    * @return attribute value: String, or null if the value not set
    * */
  def getAttributeValue(attr: String) = {
    attributes.getOrElse(attr, null)
  }

  /** Checks whether a value of the attribute is set
    *
    * @param attr name of the attribute: String
    * @return Boolean
    * */
  def hasAttribute(attr: String) = {
    attributes.contains(attr)
  }

  /** Returns the set of all attribute-value pairs associated with the Node
    *
    * @return Set[(Attribute, Value)]
    * */
  def getAttributeValuePairs = {
    attributes.map( t => new Tuple2(t._1, t._2) ).toSet
  }

  /** Adds annotation to the node
    *
    * @param ann name of the annotation: String
    * */
  def addAnnotation(ann: String) = {
    annotations += ann
  }

  /** check whether the node has annotation specified
    *
    * @param ann annotation name: String
    * @return Boolean
    * */
  def hasAnnotation(ann: String) ={
    annotations.contains(ann)
  }

  /** Gets the set of all annotations
    *
    * @return Set of all annotations of the Node: Set[String]
    * */
  def getAnnotations = {
    annotations.toSet
  }

  /** Adds the Edge to the parent Node
    *
    * @param parent parent node in the relationship [Node]
    * @param predicate type of the relationship, its originating relation [Predicate]
    * @param parentPosition position of the parent Node in the arguments of the relation    *
    * */
  def addParent(parent: Node, predicate: Predicate, parentPosition: Int) = {
    if (parent.getEntity != getEntity || parent.getDomain != getDomain) {
      parents = parents + new Edge(parent, this, predicate, parentPosition)
    }
  }

  /** Returns the set of all parent Nodes */
  def getParentNodes = {
    parents.map(_.getParent)
  }

  /** Returns the set of parent Edges */
  def getParentEdges = {
    parents
  }

  /** Adds an edge to the child node
    *
    * @param child child node: Node
    * @param predicate originating relation of the edge: Predicate
    * @param parentPosition position of the parent node in the arguments of the relation: Int
    * */
  def addChild(child: Node, predicate: Predicate, parentPosition: Int) = {
    if ((child.getEntity != getEntity || child.getDomain != getDomain) && !getParentEdges.map(_.getParent).contains(child)) {
      children = children + new Edge(this, child, predicate, parentPosition)
    }
  } //takes care of cycle edges

  /** Returns the set of all edge originating in the node */
  def getChildEdges = {
    children
  }

  /** Returns of all relations originating in this node: List[Predicate] */
  def getChildRelationships = {
    children.toList.map(_.getPredicate)
  }

  /** Returns all child nodes (including their multiplicity): List[Node] */
  def getChildNodes = {
    children.toList.map(_.getChild)
  }

  /** Returns all edge types the node participates in: List[String] */
  def getTypedEdges = {
    children.toList.map( _.getEdgeType )
  }

  /** Returns the set of all children nodes types */
  def getTypedChildren(domainType: String) = {
    children.filter(x => {
      x.getChild.getDomain == domainType
    })
  }

  /** Returns all children (including their multiplicity) that participates in a certain type of relation
    *
    * @param predicate type of the relations: Predicate
    * @return List[Node]
    * */
  def getPredicateChildren(predicate: Predicate) = {
    children.filter(_.getPredicate == predicate).toList.map(_.getChild)
  }

  /** Prints the node with a given prefix
    *
    * @param prefix prefix to insert before string representation: String
    * @return String
    * */
  def asString(prefix: String): String = {
    s"$getEntity[$getDomain]\n" +
      prefix + "|---" + getChildNodes.map(_.asString(s"$prefix|   ")).mkString(s"\n$prefix|---")
  }

  override def toString = {
    s"$getEntity[$getDomain]"
  }

  override def hashCode = {
    toString.hashCode
  }

  override def equals(other: Any) = other match {
    case other: Node => hashCode == other.hashCode
    case _ => false
  }

}

