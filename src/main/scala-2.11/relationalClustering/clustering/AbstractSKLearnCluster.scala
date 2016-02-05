package relationalClustering.clustering

import java.io.{FileWriter, File}
import relationalClustering.utils.Helper

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
  protected def getScript = {
    """import numpy as np
      |from sklearn.cluster import DBSCAN, AffinityPropagation, SpectralClustering, AgglomerativeClustering
      |import argparse
      |
      |__author__ = 'seb'
      |
      |parser = argparse.ArgumentParser(description='Read arguments for clustering.')
      |parser.add_argument('--alg', help='Algorithm to cluster [Affinity|DBscan]', nargs=1, default=['Affinity'], choices=['Affinity', 'DBscan', 'Spectral', 'Agglomerative'])
      |parser.add_argument('--input', help='filename containing distance|similarity matrix', nargs=1, required=True)
      |parser.add_argument('--output', help='filename for the resulting clustering', nargs=1, required=True)
      |parser.add_argument('--eps', help='[DBscan] epsilon parameter', nargs=1, default=[0.3], type=float)
      |parser.add_argument('--damping', help='[Affinity] dumping factor', nargs=1, default=[0.65], type=float)
      |parser.add_argument('--pref', help='[Affinity] preference', nargs=1, default=[None], type=float)
      |parser.add_argument('--k', help='[Spectral] number of clusters to find', nargs=1, default=[3], type=int)
      |
      |args = parser.parse_args()
      |
      |algorithm = args.alg[0]
      |inputFile = args.input[0]
      |outputClusters = args.output[0]
      |
      |distanceMatrix = np.loadtxt(inputFile, delimiter=";", comments="#")
      |domainObjects = map(lambda x: x.strip(), open(inputFile).readline().replace("#", "").split(";"))
      |
      |if algorithm == "DBscan":
      |    clusters = DBSCAN(eps=args.eps[0], min_samples=max(int(len(domainObjects) * 0.1), 2), metric='precomputed', algorithm='auto').fit(distanceMatrix)
      |elif algorithm == "Affinity" and args.pref:
      |    clusters = AffinityPropagation(damping=args.damping[0], affinity='precomputed', preference=args.pref[0]).fit(distanceMatrix)
      |elif algorithm == "Affinity":
      |    clusters = AffinityPropagation(damping=args.damping[0], affinity='precomputed').fit(distanceMatrix)
      |elif algorithm == "Spectral":
      |    ktoUse = min([args.k[0], np.linalg.matrix_rank(distanceMatrix) - 1])
      |    print " using k={} instead of k={}".format(ktoUse, args.k[0])
      |    clusters = SpectralClustering(n_clusters=ktoUse, affinity='precomputed').fit(distanceMatrix)
      |elif algorithm == 'Agglomerative':
      |    distance = 1.0 - np.divide(distanceMatrix, distanceMatrix.max())
      |    clusters = AgglomerativeClustering(n_clusters=args.k[0], affinity='precomputed', linkage='average').fit(distance)
      |else:
      |    print "ERROR: no {} clustering procedure, performing DBSCAN".format(algorithm)
      |    clusters = DBSCAN(eps=0.2, min_samples=max(int(len(domainObjects) * 0.1), 2), metric='precomputed', algorithm='auto').fit(distanceMatrix)
      |
      |elementsInCluster = {}
      |
      |for (element, cluster) in zip(domainObjects, clusters.labels_):
      |    if cluster not in elementsInCluster:
      |        elementsInCluster[cluster] = []
      |    elementsInCluster[cluster].append(element)
      |
      |writer = open(outputClusters, 'w')
      |
      |oneBig = []
      |
      |for item in elementsInCluster:
      |    if len(elementsInCluster[item]) > 1:
      |        writer.write("{}=".format(item) + "{" + ";".join(elementsInCluster[item]) + "}\n")
      |    else:
      |        oneBig.append(elementsInCluster[item][0])
      |
      |if len(oneBig) > 0:
      |    writer.write("{}=".format(len(elementsInCluster)) + "{" + ";".join(oneBig) + "}\n")
      |writer.close()
      |
      |
    """.stripMargin
  }

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

  /** Parses the resulting file and return the set of clusters */
  protected def readClusters: Set[List[String]] = {
    var clusters = collection.mutable.Set[List[String]]()

    Helper.readFile(getResultFile).foreach( line => {
      clusters = clusters + line.split("""\{""")(1).replace("}", "").split(";").toList
    })

    clusters.toSet
  }

}