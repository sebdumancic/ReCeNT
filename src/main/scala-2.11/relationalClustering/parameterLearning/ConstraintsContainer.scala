package relationalClustering.parameterLearning

import relationalClustering.neighbourhood.{NeighbourhoodGraph, NodeRepository}
import relationalClustering.representation.domain.KnowledgeBase

import scala.io.Source

/**
  * Created by seb on 13.09.16.
  */
class ConstraintsContainer(protected val filename: String,
                           protected val domain: String,
                           protected val knowledgeBase: KnowledgeBase,
                           protected val treeDepth: Int) {


  protected val mustLink = collection.mutable.Set[(NeighbourhoodGraph, NeighbourhoodGraph)]()
  protected val cannotLink = collection.mutable.Set[(NeighbourhoodGraph, NeighbourhoodGraph)]()
  protected val constraintRegex = """(.*) (.*) (.*)""".r
  val nodeRepo = new NodeRepository(knowledgeBase)

  if (filename.length > 1) {
    readConstraints()
  }


  protected def readConstraints() = {
    val inputFile = Source.fromFile(filename)

    try {
      inputFile.getLines().filter(_.length > 3).foreach(line => {
        val constraintRegex(obj1, obj2, t) = line
        val ng1 = new NeighbourhoodGraph(obj1, domain, treeDepth, knowledgeBase, nodeRepo)
        val ng2 = new NeighbourhoodGraph(obj2, domain, treeDepth, knowledgeBase, nodeRepo)

        t match {
          case x if x == "ML" =>
            mustLink.+=((ng1, ng2))
          case x if x == "CL" =>
            cannotLink.+=((ng1, ng2))
        }
      })
    }
    catch {
      case e: Exception => throw new Exception(s"ConstraintsContainer::readConstraints : cannot read file $filename; $e")
    }
    finally {
      inputFile.close()
    }
  }

  def addMustLink(nt1: NeighbourhoodGraph, nt2: NeighbourhoodGraph): Unit = {
    mustLink.+=((nt1, nt2))
  }

  def addMustLink(obj1: String, obj2: String): Unit = {
    val ng1 = new NeighbourhoodGraph(obj1, domain, treeDepth, knowledgeBase, nodeRepo)
    val ng2 = new NeighbourhoodGraph(obj2, domain, treeDepth, knowledgeBase, nodeRepo)
    addMustLink(ng1, ng2)
  }

  def addCannotLink(nt1: NeighbourhoodGraph, nt2: NeighbourhoodGraph): Unit = {
    cannotLink.+=((nt1, nt2))
  }

  def addCannotLink(obj1: String, obj2: String): Unit = {
    val ng1 = new NeighbourhoodGraph(obj1, domain, treeDepth, knowledgeBase, nodeRepo)
    val ng2 = new NeighbourhoodGraph(obj2, domain, treeDepth, knowledgeBase, nodeRepo)
    addCannotLink(ng1, ng2)
  }

  def getMustLink = {
    mustLink.toSet
  }

  def getCannotLink = {
    cannotLink.toSet
  }
}
