package relationalClustering.representation.definition

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 27.03.17.
  */
class DefinitionMinerTopK(override protected val clustering: Clustering,
                          protected val k: Int) extends DefinitionMiner(clustering) {

  override def getDefinitions(weights: List[Double]): Map[String, List[VertexClusterDefinition]] = {
    clustering.getClusters.foldLeft(Map[String, List[VertexClusterDefinition]]())((acc, clust) => {
      var definitions: List[VertexClusterDefinition] = List()
      if (weights.head > 0.0) {
        definitions = definitions :+ getAttributeDefinition(clust).topK(k)
      }
      if (weights(1) > 0.0) {
        definitions = definitions :+ getNeighbourhoodAttributeDefinition(clust).topK(k)
      }
      if (weights(3) > 0.0) {
        definitions = definitions :+ getIdentitiesDefinition(clust).topK(k)
      }
      if (weights(4) > 0.0) {
        definitions = definitions :+ getEdgeDistributionDefinition(clust).topK(k)
      }
      acc + (clust.getClusterName -> definitions.filterNot(_.isEmpty))
    })
  }

}
