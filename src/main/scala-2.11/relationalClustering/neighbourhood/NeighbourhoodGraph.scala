package relationalClustering.neighbourhood

import relationalClustering.representation.domain.{KnowledgeBase, Predicate}
import relationalClustering.utils.Settings

/**
  * Implements neighbourhood graph functionality that is used to estimate indirect links measure within similarity measure
  *
  * @constructor creates a neighbourhood graph for a given object name and domain, with the given depth and knowledge base, from provided NodeRepository
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
  var vertexIdentityCache: collection.mutable.Map[Int, collection.mutable.Map[String, List[String]]] = null //collection.mutable.Map[Int, collection.mutable.Map[String, List[String]]]()
  var edgeDistributionCache: Map[Int, List[String]] = null
  var attributeDistributionCache: Map[Int, collection.mutable.Map[String, List[(String,String)]]] = null
  var clauseCache: Set[List[String]] = null
  var edgeCache: Set[Edge] = null
  construct()

  /** Secondary constructor if one wants to use the local [[NodeRepository]] - takes care of recursive edges explicitly
    *
    * @constructor creates a neighbourhood graph for a given object name and domain, with the given depth and knowledge base
    * @param rootObject name of the root object: [[String]]
    * @param domain domain of the root element: [[String]]
    * @param depth depth of the neighbourhood graph: [[Int]]
    * @param kBase knowledge base to construct: [[KnowledgeBase]]
    * */
  def this(rootObject: String, domain: String, depth: Int, kBase: KnowledgeBase) = {
    this(rootObject, domain, depth, kBase, new LocalNodeRepository(kBase))
  }

  /** Clears all the cached values */
  def clearCache() = {
    vertexIdentityCache = null
    edgeDistributionCache = null
    attributeDistributionCache = null
    nodeRepo.clearCache()
  }

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
  def collectVertexIdentity(): collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]] = {
    if (vertexIdentityCache != null) {
      return vertexIdentityCache
    }

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

    vertexIdentityCache = resultSummary

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
  def getEdgeDistribution: Map[Int, List[String]] = {
    if (edgeDistributionCache != null) {
      return edgeDistributionCache
    }

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

    edgeDistributionCache = levelWise.toMap

    levelWise.toMap
  }

  /** Collects the attribute value information, as well as annotations, per level and vertex type
    *
    * @return Map[Int, Map[String, List[(String,String)] ] ] level -> type -> list of attribute-values
    * */
  def getAttributeValueDistribution: Map[Int, collection.mutable.Map[String, List[(String,String)]]] = {
    if (attributeDistributionCache != null) {
      return attributeDistributionCache
    }

    val returnData = collection.mutable.Map[Int, collection.mutable.Map[String, List[(String,String)]]]()

    var currentLevel = 0
    var frontier = Set[Node](getRoot)
    var newFrontier = Set[Node]()

    while (currentLevel <= getMaxDepth) {

      if (!returnData.contains(currentLevel)) {
        returnData(currentLevel) = collection.mutable.Map[String, List[(String,String)]]()
      }

      // expand children
      frontier.foreach( cNode => {
        cNode.getChildNodes.toSet.filter(_.getEntity != getRoot.getEntity).foreach( child => {

          if (!returnData(currentLevel).contains(child.getDomain)) {
            returnData(currentLevel)(child.getDomain) = List[(String, String)]()
          }

          // add attributes to collection
          returnData(currentLevel)(child.getDomain) = returnData(currentLevel)(child.getDomain) ++: child.getAttributeValuePairs.toList

          //add child to the frontier
          newFrontier = newFrontier + child
        })
      })

      frontier = newFrontier.map( x => x)
      newFrontier = Set[Node]()
      currentLevel += 1
    }

    attributeDistributionCache = returnData.toMap

    returnData.toMap
  }

  /** Checks whether a clause is valid -> if there are more than two distinct variables, the edge should be present
    *
    * @param clause a clause to check
    * @param edgeName edge that should be present
    * @return true if the clause is valid
    * */
  protected def checkValidityOfClause(clause: List[String], edgeName: String): Boolean = {
    val vars = clause.flatMap(x => x.split("""\(""")(1).split(",")).filter(x => x == x.toLowerCase).distinct

    if (!vars.contains("x")) { false }
    else if (vars.length == 1 && vars.contains("x")) { true }
    else if (vars.length > 1 && vars.contains("x") && clause.map( _.startsWith(edgeName)).reduce(_ || _)) {
      true
    }
    else {false}
  }

  /** Returns the set of all clauses that can be constructed from this neighbourhood graph
    *
    * @param maxLength maximal length of a clause
    * */
  def getClauses(maxLength: Int): Set[List[String]] = {

    if (clauseCache != null) {
      return clauseCache
    }

    //attribute clauses
    val rootAttributeClauses = (getRoot.getAnnotations.map( x => s"${x._1}(x)") ++ getRoot.getAttributeValuePairs.map( x => s"${x._1}(x,${x._2})")).toList

    // go over existing edges types involving the root element
    clauseCache = getRoot.getChildEdges.map(_.getPredicate).foldLeft(Set[List[String]]())( (acc, pred) => {

      // go over existing edges
      acc ++ pred.getTrueGroundings.filter( _.contains(getRoot.getEntity)).foldLeft(Set[List[String]]())( (acc_1, edge) => {

        val nodes = edge.zipWithIndex.filter(_._1 != getRoot.getEntity).map(node => (nodeRepo.getNode(node._1, pred.getDomains(node._2)), node._2))
        val edgeName = s"${pred.getName}(${edge.zipWithIndex.map(ar => if (ar._1 == getRoot.getEntity) "x" else s"v${ar._2}").mkString(",")})"
        val allAtoms = nodes.flatMap(x => {
          x._1.getAnnotations.map( an => s"${an._1}(v${x._2})") ++ x._1.getAttributeValuePairs.map( at => s"${at._1}(v${x._2},${at._2})")
        }) ++ (rootAttributeClauses :+ edgeName)

        acc_1 ++ (2 to maxLength).foldLeft(Set[List[String]]())( (acc_ii, len) => {
          acc_ii ++  allAtoms.combinations(len).filter(x => checkValidityOfClause(x, pred.getName.split("""\(""").head)).map( x => x.sorted)
        })

      })
    })

    clauseCache
  }

  /** Returns the set of all edges in a neighbourhood graph */
  def getAllEdges: Set[Edge] = {
    if (edgeCache != null){
      return edgeCache
    }
    val edges = collection.mutable.Set[Edge]()

    var currentDepth = 0
    var frontier = Set[Node](getRoot)
    var newFrontier = Set[Node]()

    while (currentDepth <= getMaxDepth) {

      frontier.foreach( cNode => {
        cNode.getChildEdges.foreach( child => {
          edges += child
          if (child.getChild != getRoot) {
            newFrontier = newFrontier + child.getChild
          }
        })
      })

      frontier = newFrontier.map(x => x)
      newFrontier = Set[Node]()

      currentDepth += 1
    }

    edgeCache = edges.toSet
    edgeCache
  }
}
