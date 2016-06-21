package relationalClustering.utils

import scala.io.Source

/**
 *
 * A helper object containing small functions that does not belong to any specific objects
 * Created by seb on 29.06.15.
 */
object Helper {

  def bool2int(value: Boolean): Int = {
    if (value) 1 else 0
  }

  def readFile(filename: String) = {
    val fileSource = Source.fromFile(filename)
    val lines = fileSource.getLines().mkString("\n")
    fileSource.close()
    lines.split("\n")
  }

  /** Finds all possible matches of template within allDomains
    *
    * @param template list of domain of interest (sub-hyperedge)
    * @param allDomains full hyperedges
    *
    * template = (dom1, dom2), allDomains = (dom1, dom2) => ((0,1))
    * template = (dom1, dom2), allDomains = (dom1, dom1, dom2) => ((0,2), (1,2))
    * */
  def extractSubHyperedge(template: List[String], allDomains: List[String]) = {
    val domMatch = template.map(d => allDomains.zipWithIndex.filter(_._1 == d).map(_._2).map(t => List(t)))
    domMatch.reduceLeft( (x,y) => { for { xs <-x; ys <- y} yield xs ::: ys} ).filter(m => m == m.sorted)
  }

}
