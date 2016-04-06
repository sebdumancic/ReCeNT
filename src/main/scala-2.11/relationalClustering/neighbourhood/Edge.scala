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

  /** Returns true is it is a unary Edge (child is null)*/
  def isUnary = {
    childNode == null
  }

  /** Returns the arity of an edge */
  def arity = {
    childNode == null match {
      case true => 1
      case false => 2
    }
  }

  /** Returns vertices as a list */
  def getVertices = {
    isUnary match {
      case true => List(getParent)
      case false => List(getParent, getChild)
    }
  }

  override def toString = {
    childNode == null match {
      case true => s"$getParent --[${getPredicate.getName}],[$getParentPosition]"
      case false => s"$getParent --[${getPredicate.getName}],[$getParentPosition]--> $getChild"
    }
  }

  override def equals(other: Any) = other match {
    case other: Edge => hashCode == other.hashCode
    case _ => false
  }

  override def hashCode = {
    toString.hashCode
  }
}
