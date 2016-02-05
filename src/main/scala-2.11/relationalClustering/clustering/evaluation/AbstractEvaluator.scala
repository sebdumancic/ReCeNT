package relationalClustering.clustering.evaluation

/**
  * Created by seb on 05.02.16.
  */
abstract class AbstractEvaluator(protected val rootFolder: String = "./tmp") {

  protected def getRoot = {
    rootFolder
  }
}
