package relationalClustering.neighbourhood

import relationalClustering.representation.domain.Predicate

/**
  * Edge functionality for the Neighbourhood graph
  *
  * @constructor create en edge between two node, taking the type and position of the parent node in the arguemnts of the relation
  * @param parentNode parent node in the edge: Node
  * @param childNode child node in the edge: Node
  * @param relationshipPredicate relation that forms the edge (denotes type): Predicate
  * @param parentPositionInGrounding position of the parent node in the relation: Int
  *
  * Created by seb on 01.02.16.
  */
class Edge(protected val parentNode: Node,
           protected val childNode: Node,
           protected val relationshipPredicate: Predicate,
           protected val parentPositionInGrounding: Int) {

  /** Returns the child node: Node */
  def getChild = {
    childNode
  }

  /** Returns the parent node: Node */
  def getParent = {
    parentNode
  }

  /** Returns the relation predicate: Predicate */
  def getPredicate = {
    relationshipPredicate
  }

  /** Returns the position of the parent node in relation: Int */
  def getParentPosition = {
    parentPositionInGrounding
  }

  /** Return the edge type */
  def getEdgeType = {
    s"${getPredicate.getName}$getParentPosition"
  }

  override def toString = {
    s"$getParent --[${getPredicate.getName}],[$getParentPosition]--> $getChild"
  }

  override def equals(other: Any) = other match {
    case other: Edge => hashCode == other.hashCode
    case _ => false
  }

  override def hashCode = {
    toString.hashCode
  }
}
