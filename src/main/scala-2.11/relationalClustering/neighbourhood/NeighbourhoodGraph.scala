package relationalClustering.neighbourhood

import relationalClustering.representation.{Predicate, KnowledgeBase}
import relationalClustering.utils.Settings

/**
  * Implements neighbourhood graph functionality that is used to estimate indirect links measure within similarity measure
  *
  * @constructor creates a neighbourhood graph for a given object name and domain, with the given depth and knowledge base
  * @param rootObject name of the root object: [[String]]
  * @param domain domain of the root element: [[String]]
  * @param depth depth of the neighbourhood graph: [[Int]]
  * @param kBase knowledge base to construct: [[KnowledgeBase]]
  * @param nodeRepo node repository: [[NodeRepository]]
  *
  * Created by seb on 16.10.15.
 */
class NeighbourhoodGraph(protected val rootObject: String,
                         protected val domain: String,
                         protected val depth: Int,
                         protected val kBase: KnowledgeBase,
                         protected val nodeRepo: NodeRepository) {

  val root = nodeRepo.getNode(rootObject, domain) //new Node(rootObject, domain)
  construct()

  /** Return the knowledge base of an NG*/
  def getKnowledgeBase = {
    kBase
  }

  /** Returns the domain of the root element */
  def getRootDomain = {
    domain
  }

  /** Return the root element */
  def getRoot = {
    root
  }

  /** Return the depth of the NG */
  def getMaxDepth = {
    depth
  }

  /** Returns the root elements attributes (including annotations)*/
  def getRootAttributes = {
    getRoot.getAttributeValuePairs ++ getRoot.getAnnotations
  }

  /** Constructs all the edges for the node, by traversing all the hyperedge predicates
    *
    * @param node node to create edges for: Node
    * */
  private def constructNodeEdges(node: Node) = {
    //includes only relationship into account here
    getKnowledgeBase.getPredicateNames.map(kBase.getPredicate).filter(_.getRole == Settings.ROLE_HYPEREDGE).filter(_.getDomains.contains(node.getDomain)).foreach(pred => {
      constructEdgeGivenPredicate(node, pred)
    })
  }

  /** Construct the edges for the node, given the edge type
    *
    * @param node node to create edges for: Node
    * @param predicate type of the relation: Predicate
    * */
  private def constructEdgeGivenPredicate(node: Node, predicate: Predicate) = {

    //iterate over potential bindings
    predicate.getDomains.zipWithIndex.filter(_._1 == node.getDomain).foreach(binding => {
      //find children - true groundings
      predicate.getTrueGroundings.filter(_ (binding._2) == node.getEntity).foreach(ground => {
        val focusNodePosition = ground.indexOf(node.getEntity)
        ground.zipWithIndex.filterNot(_._1 == node.getEntity).foreach(child => {
          //filterNot - no recursive links allowed

          //connect the nodes
          val childNode = nodeRepo.getNode(child._1, predicate.getDomains(child._2))
          node.addChild(childNode, predicate, focusNodePosition)
          childNode.addParent(node, predicate, focusNodePosition)
        })
      })
    })
  }

  /** Constructs the Neighbourhood graph */
  def construct() = {
    var frontier = List[Node](getRoot)
    var currentDepth = 0
    var newFrontier = List[Node]()

    while (currentDepth <= getMaxDepth) {

      //extend current frontier
      frontier.foreach(cNode => {
        constructNodeEdges(cNode)
        newFrontier = newFrontier ++ cNode.getChildNodes
      })

      //update frontiers (newFrontier becomes current frontier, empty newFrontier)
      //frontier = List[Node]()
      frontier = newFrontier.map(x => x)
      newFrontier = List[Node]()

      currentDepth += 1
    }
  }

  /** Collects the vertex identity information in the Neighbourhood graph
    *
    * @return Map[Int, Map[String, List[String] ] ] (level -> domain -> list of objects)
    *
    * */
  def collectVertexIdentity() = {
    val resultSummary = collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]]()
    var currentLevel = 0
    var frontier = List[Node](getRoot)
    var newFrontier = List[Node]()

    while (currentLevel <= getMaxDepth) {

      if (!resultSummary.contains(currentLevel)) {
        resultSummary(currentLevel) = collection.mutable.Map[String, List[String]]()
      }

      //expand all nodes in a frontier, store the information (if the root element in encountered again, exclude it)
      frontier.foreach(cNode => {
        cNode.getChildNodes.filter( _.getEntity != getRoot.getEntity).foreach(child => {
          // condition to prevent expanding on the root element again
          newFrontier = newFrontier :+ child

          if (!resultSummary(currentLevel).contains(child.getDomain)) {
            resultSummary(currentLevel)(child.getDomain) = List[String]()
          }
          resultSummary(currentLevel)(child.getDomain) = resultSummary(currentLevel)(child.getDomain) :+ child.getEntity
        })
      })
      frontier = newFrontier.map(x => x)
      newFrontier = List[Node]()
      currentLevel += 1
    }
    resultSummary
  }

  /** Collects the vertex identity information on a certain level
    *
    * @param level level of interest: Int
    * */
  def collectVertexIdentity(level: Int): Map[String, List[String]] = {
    val allInformation = collectVertexIdentity(); allInformation(level).toMap
  }

  /** Return the root predicates (relations) */
  def getRootPredicates = {
    getRoot.getChildEdges.map(_.getPredicate)
  }

  /** Returns the edge type of the root element (including multiplicity) */
  def getRootEdgeTypes = {
    getRoot.getChildEdges.toList.map( _.getEdgeType )
  }

  override def toString = {
    getRoot.asString("")
  }

  /** Collects edge types over levels
    *
    * @return Map[Int, List[String] ]; depth -> list of edge types
    * */
  def getEdgeDistribution = {
    val levelWise = collection.mutable.Map[Int, List[String]]() // depth -> List[Edge distribution]

    var currentDepth = 0
    var frontier = Set[Node](getRoot)
    var newFrontier = Set[Node]()

    while (currentDepth <= getMaxDepth) {

      levelWise(currentDepth) = List[String]()

      frontier.foreach( cNode => {
        levelWise(currentDepth) = levelWise(currentDepth) ++ cNode.getChildEdges.toList.map( _.getEdgeType )

        //expand new frontier (excluding the root element)
        cNode.getChildNodes.filter( _.getEntity != getRoot.getEntity).foreach(child => {
          newFrontier = newFrontier + child
        })
      })

      frontier = newFrontier.map(x => x)
      newFrontier = Set[Node]()
      currentDepth = currentDepth + 1
    }

    levelWise.toMap
  }

  /** Collects the attribute value information, as well as annotations, per level and vertex type
    *
    * @return Map[Int, Map[String, List[(String,String)] ] ] level -> type -> list of attribute-values
    * */
  def getAttributeValueDistribution = {
    val returnData = collection.mutable.Map[Int, Map[String, List[(String,String)]]]()

    var currentDepth = 0
    var frontier = getRoot.getChildNodes.toSet
    var newFrontier = Set[Node]()

    // +1 necessary so that depth 0 include all immediate connections
    while (currentDepth <= getMaxDepth + 1) {
      val levelContent = collection.mutable.Map[String, List[(String,String)]]()
      val domains = frontier.map( _.getDomain )

      //collecting over nodes at current level
      domains.foreach( dom => {
        levelContent(dom) = frontier.filter( _.getDomain == dom ).foldLeft(List[(String,String)]())( (acc, cNode) => {
          //expand the frontier (excluding the root element if encountered again)
          cNode.getChildNodes.filter( _.getEntity != getRoot.getEntity).foreach(child => {
            newFrontier = newFrontier + child
          })

          acc ++ cNode.getAttributeValuePairs.toList ++ cNode.getAnnotations.toList
        })
      })

      //put content in returnData
      returnData(currentDepth) = levelContent.toMap

      frontier = newFrontier.map( x => x )
      newFrontier = Set[Node]()
      currentDepth = currentDepth + 1
    }

    returnData.toMap
  }
}
