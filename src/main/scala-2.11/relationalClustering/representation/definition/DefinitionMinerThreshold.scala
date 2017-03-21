package relationalClustering.representation.definition

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 21.03.17.
  */
class DefinitionMinerThreshold(override protected val clustering: Clustering,
                               protected val minSupport: Double,
                               protected val maxDeviance: Double) extends DefinitionMiner(clustering) {

  override def getDefinitions(weights: List[Double]): Map[String, List[VertexClusterDefinition]] = {
    clustering.getClusters.foldLeft(Map[String, List[VertexClusterDefinition]]())((acc, clust) => {
      var definitions: List[VertexClusterDefinition] = List()
      if (weights.head > 0.0) {
        definitions = definitions :+ getAttributeDefinition(clust).withFilter(minSupport, maxDeviance)
      }
      if (weights(1) > 0.0) {
        definitions = definitions :+ getNeighbourhoodAttributeDefinition(clust).withFilter(minSupport, maxDeviance)
      }
      if (weights(3) > 0.0) {
        definitions = definitions :+ getIdentitiesDefinition(clust).withFilter(minSupport, maxDeviance)
      }
      if (weights(4) > 0.0) {
        definitions = definitions :+ getEdgeDistributionDefinition(clust).withFilter(minSupport, maxDeviance)
      }
      acc + (clust.getClusterName -> definitions.filterNot(_.isEmpty))
    })
  }

}
