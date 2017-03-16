package relationalClustering.clustering.algo

/**
  * Created by seb on 10.04.16.
  */
class DBScan( protected val eps: Double,
              override protected val rootFolder: String) extends AbstractSKLearnCluster("dbscan", rootFolder) {

  protected def getResultFile = {
    s"$getRoot/result.txt"
  }

  protected def prepareParameters(inputFile: String, k: Int) = {
    val pars = collection.mutable.Map[String,String]()

    pars("--alg") = "DBscan"
    pars("--input") = inputFile
    pars("--output") = getResultFile
    pars("--eps") = s"$eps"
    pars.toMap
  }

}
