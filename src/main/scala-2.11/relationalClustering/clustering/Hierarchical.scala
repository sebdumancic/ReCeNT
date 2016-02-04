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
}
