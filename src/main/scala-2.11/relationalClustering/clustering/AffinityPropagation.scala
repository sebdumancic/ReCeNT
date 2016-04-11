package relationalClustering.clustering

/**
  * Created by seb on 11.04.16.
  */
class AffinityPropagation(override protected val rootFolder: String,
                          protected val damping: Double,
                          protected val preference: Double) extends AbstractSKLearnCluster("spectral", rootFolder) {

  protected def getResultFile = {
    s"$getRoot/result.txt"
  }

  protected def prepareParameters(inputFile: String, k: Int) = {
    val pars = collection.mutable.Map[String,String]()

    pars("--alg") = "Affinity"
    pars("--input") = inputFile
    pars("--damping") = s"$damping"
    pars("--pref") = s"$preference"
    pars("--output") = getResultFile

    pars.toMap
  }

}
