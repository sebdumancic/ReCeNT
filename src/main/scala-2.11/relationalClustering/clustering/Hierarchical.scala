package relationalClustering.clustering

import relationalClustering.utils.Helper

/**
  * Created by seb on 04.02.16.
  */
class Hierarchical(protected val linkage: String,
                   override protected val rootFolder: String) extends AbstractSKLearnCluster(rootFolder){

  protected def getResultFile = {
    s"$getRoot/result.txt"
  }

  protected def prepareParameters(inputFile: String, k: Int) = {
    val pars = collection.mutable.Map[String,String]()

    pars("--alg") = "Agglomerative"
    pars("--input") = inputFile
    pars("--k") = s"$k"
    pars("--output") = getResultFile

    pars.toMap
  }

  protected def readClusters = {
    var clusters = collection.mutable.Set[List[String]]()

    Helper.readFile(getResultFile).foreach( line => {
      clusters = clusters + line.split("""\{""")(1).replace("}", "").split(";").toList
    })

    clusters.toSet
  }

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
}
