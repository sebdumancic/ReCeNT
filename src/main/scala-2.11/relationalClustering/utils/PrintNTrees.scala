package relationalClustering.utils

import java.io.File

import relationalClustering.neighbourhood.{NeighbourhoodGraph, NodeRepository}
import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 07.04.16.
  */
object PrintNTrees {

  def saveAll(domain: String, KB: KnowledgeBase, depth: Int, folder: String) = {
    val repo = new NodeRepository(KB)
    val fold = new File(s"$folder/gspan")
    fold.mkdir()

    KB.getDomain(domain).getElements.foreach( elem => {
      saveIndividual(elem, domain, depth, repo, s"$folder/gspan/$elem.gspan", KB)
    })
  }

  protected def saveIndividual(element: String, domain: String, depth: Int, nodeRepository: NodeRepository, filename: String, kBase: KnowledgeBase) = {
    val nt = new NeighbourhoodGraph(element, domain, depth, kBase, nodeRepository)
    nt.saveAsGspan(filename)
  }

}
