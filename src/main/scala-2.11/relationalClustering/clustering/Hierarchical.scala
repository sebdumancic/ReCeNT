package relationalClustering.clustering

/**
  * Created by seb on 04.02.16.
  */
class Hierarchical(protected val linkage: String,
                   override protected val rootFolder: String) extends AbstractSKLearnCluster("hierarchical",rootFolder){

  protected def getResultFile = {
    s"$getRoot/result.txt"
  }

  protected def prepareParameters(inputFile: String, k: Int) = {
    val pars = collection.mutable.Map[String,String]()

    pars("--alg") = "Agglomerative"
    pars("--input") = inputFile
    pars("--k") = s"$k"
    pars("--output") = getResultFile
    pars("--linkage") = linkage

    pars.toMap
  }
}
