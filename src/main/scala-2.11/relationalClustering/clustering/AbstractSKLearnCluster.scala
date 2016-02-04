package relationalClustering.clustering

import java.io.{FileWriter, File}
import scala.sys.process._

/**
  * Created by seb on 04.02.16.
  */
abstract class AbstractSKLearnCluster(protected val algName: String,
                                      protected val rootFolder: String = "./tmp") extends AbstractCluster {

  // makes sure the directory exists
  val dir = new File(rootFolder)
  if (!dir.exists()) { dir.mkdir() }
  prepareScript()

  /** Returns the root folder */
  def getRoot = {
    rootFolder
  }

  /** Returns file with results */
  protected def getResultFile: String

  /** Returns String containing the sci-kit command */
  protected def getScript: String

  /** Parses the resulting file and return the set of clusters */
  protected def readClusters: Set[List[String]]

  /** Prepares python clustering script */
  protected def prepareScript() = {
    val writer = new FileWriter(s"$getRoot/${algName}_script.py")
    writer.write(getScript)
    writer.close()
  }

  /** Returns the command to run
    *
    * @param parameters parameters for the script; printed in format ' key value'
    * */
  protected def command(parameters: Map[String, String]) = {
    s"python $getRoot/${algName}_script.py" + parameters.map( par => s" ${par._1} ${par._2}").mkString
  }

  /** Returns the map with parameters for clustering
    *
    * @param k number of clusters
    * */
  protected def prepareParameters(inputFile: String, k: Int): Map[String, String]

  def clusterFromFile(filename: String, k: Int) = {
    command(prepareParameters(filename, k)).!(ProcessLogger(line => println(line), line => println(s"CLUSTER ERROR: $line")))

    readClusters
  }

}
