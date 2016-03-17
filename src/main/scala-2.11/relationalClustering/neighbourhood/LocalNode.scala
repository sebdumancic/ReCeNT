package relationalClustering.neighbourhood

import relationalClustering.representation.domain.Predicate

/**
  * Created by seb on 10.02.16.
  */
class LocalNode(override protected val entity: String,
                override protected val domain: String) extends Node(entity, domain) {

  override def addChild(child: Node, predicate: Predicate, parentPosition: Int) = {
    if ((child.getEntity != getEntity || child.getDomain != getDomain) && !getParentEdges.map(_.getParent).contains(child)) {
      children = children + new Edge(this, child, predicate, parentPosition)
    }
  }

}
