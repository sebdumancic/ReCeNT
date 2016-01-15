package relationalClustering

import java.io.FileWriter

import org.clapper.argot.ArgotParser
import relationalClustering.representation.KnowledgeBase
import relationalClustering.utils.{PredicateDeclarations, Helper}

import scala.sys.process._

/**
  * Created by seb on 02.12.15.
  */
object RelationalClustering {


  //parser specification
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("RelationalClustering.jar", preUsage = Some("Version 1.0"))
  val dbs = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
  val head = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val depth = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val normalize = parser.flag[Boolean](List("normalize"), "should the distances be normalized")
  val rootFolder = parser.option[String](List("root"), "filePath", "folder to place files in")
  val k = parser.option[Int](List("k"), "n", "number of clusters to create")
  val logFile = parser.option[String](List("logFile"), "filePath", "log file")
  val query = parser.option[String](List("query"), "comma-separated list", "list of query domains")
  val labels = parser.multiOption[String](List("labels"), "filePath", "file with labels [format: Class()]")
  val labelsHeader = parser.option[String](List("labelsDomain"), "domain definition of label predicates", "header for the knowledge base(s); specification of logical predicates")
  val scaleFactors = parser.option[String](List("scale"), "Array[Double]", "comma-separated list of scaling factors (4 numbers that sum to 1)")
  val overlapType = parser.option[String](List("overlap"), "[union|min|max|histogram]", "normalization of overlapping measure")
  val algorithm = parser.option[String](List("algorithm"), "[Spectral|Agglomerative|Affinity]", "algorithm to perform clustering")
  val asDB = parser.flag[Boolean](List("asDB"), "should results be written in a database format (clusters_{query}.db)")
  val neville = parser.flag[Boolean](List("asNeville"), "Calculate Neville's similarity matrix [if set, overrides all other settings]")
  val declarationFile = parser.option[String](List("declarations"), "file path", "[optional] file containing declarations of predicates")
  val ribl = parser.flag[Boolean](List("asRIBL"), "Calculate RIBL's similarity matrix [if set, overrides all other settings]")

  val ariScript =
    """
      |from sklearn.metrics.cluster import adjusted_rand_score
      |import argparse
      |import numpy as np
      |
      |parser = argparse.ArgumentParser(description='Read arguments for clustering.')
      |parser.add_argument('--input', help='filename containing distance|similarity matrix', nargs=1, required=True)
      |
      |args = parser.parse_args()
      |inputFile = args.input[0]
      |
      |clustersAndLabels = np.loadtxt(inputFile, delimiter=";", comments="#")
      |randScore = adjusted_rand_score(clustersAndLabels[:,1], clustersAndLabels[:,0])
      |writer = open(inputFile.replace(".txt", "_result.txt"), "w")
      |writer.write("ARI: {}".format(randScore))
      |writer.close()
    """.stripMargin

  private def createAriScript() = {
    val writer = new FileWriter(rootFolder.value.getOrElse(".") + "/ari.py")
    writer.write(ariScript)
    writer.close()
  }

  def main (args: Array[String]) {
    parser.parse(args)
    require(head.hasValue, "no header specified")
    require(dbs.hasValue, "no databases specified")
    require(query.hasValue, "query not specified")
    require(!neville.value.getOrElse(false) || !ribl.value.getOrElse(false), "Neville and RIBL flags cannot be used at the same time")

    val header = Helper.readFile(head.value.get).mkString("\n")

    val declarations = declarationFile.value.orNull match {
      case null => null
      case file: String => new PredicateDeclarations(file)
    }

    val knowledgeBase = new KnowledgeBase(dbs.value, header)
    val clusteringAlgorithm = new RelationalClusteringInitialization(knowledgeBase,
                                                                     depth.value.getOrElse(2),
                                                                     true,
                                                                     rootFolder.value.getOrElse("."),
                                                                     k.value.getOrElse(3),
                                                                     logFile.value.getOrElse("./clustering.log"),
                                                                     scaleFactors.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(",").map( _.toDouble),
                                                                     overlapType.value.getOrElse("histogram"),
                                                                     algorithm.value.getOrElse("Spectral"),
                                                                     neville.value.getOrElse(false),
                                                                     ribl.value.getOrElse(false),
                                                                     declarations)

    val startTime = System.currentTimeMillis()
    val results = clusteringAlgorithm.getAllClusters(query.value.get.split(",").toList, subSample = false, proportion = 1.0)
    println("- Writing clusters \n")
    for ( cluster <- results.zipWithIndex) {
      print("CLUSTER " + cluster._2 + ":   ")
      println(cluster._1.map( _.replace(",", ":")).mkString(", "))
      println("****************************************")
    }

    if (asDB.value.getOrElse(false)) {
      val domainDesc = query.value.get.replace(",", "")
      val domainArgs = query.value.get
      val fileDB = new FileWriter(rootFolder.value.getOrElse(".") + s"/clusters_$domainDesc.db")
      fileDB.write((0 until k.value.getOrElse(3)).map(x => s"Cluster$x" + s"_$domainDesc($domainArgs)").mkString("\n") + "\n\n")
      for ( cluster <- results.zipWithIndex) {
        fileDB.write(cluster._1.map( "Cluster" + cluster._2 + s"_$domainDesc(" + _ + ")").mkString("\n") + "\n")
      }
      fileDB.close()
    }

    println("Clustering took: " + (System.currentTimeMillis() - startTime) + "ms")
    if (labels.hasValue) {
      createAriScript()
      require(labelsHeader.hasValue, "no header for label predicates")

      val labelsKnowledgeBase = new KnowledgeBase(labels.value, Helper.readFile(labelsHeader.value.get).mkString("\n"))
      val sortedPredicates = labelsKnowledgeBase.getPredicateNames
      require( sortedPredicates.map( labelsKnowledgeBase.getPredicate ).count( _.arity == 1) == sortedPredicates.size, "not all label predicates are 1-arity")

      val trueLabels = collection.mutable.Map[String, Int]()
      sortedPredicates.map( labelsKnowledgeBase.getPredicate ).foreach( predicate => {
        predicate.getTrueGroundings.foreach( element => { trueLabels(element.mkString(",")) = sortedPredicates.indexOf(predicate.getName) })
      })

      val tmpCalculations = new FileWriter(rootFolder.value.getOrElse(".") + "/ari_calc.txt")

      for (cluster <- results.zipWithIndex) {
        tmpCalculations.write( cluster._1.filter( x => trueLabels.contains(x)).map( x => cluster._2 + ";" + trueLabels(x)).mkString("\n") + "\n")
      }
      tmpCalculations.close()

      val procVal = ("python " + rootFolder.value.getOrElse(".") + "/ari.py --input " + rootFolder.value.getOrElse(".") + "/ari_calc.txt").!

      println(Helper.readFile(rootFolder.value.getOrElse(".") + "/ari_calc_result.txt").mkString("\n"))
    }
  }

}
