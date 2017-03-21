package relationalClustering

import org.clapper.argot.{ArgotParser, MultiValueOption, SingleValueOption}
import relationalClustering.aggregators._
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, UnionBagSimilarity}
import relationalClustering.clustering.algo.{AffinityPropagation, DBScan, Hierarchical, Spectral}
import relationalClustering.clustering.evaluation._
import relationalClustering.clustering.selection.{IncreaseSaturationCut, ModelBasedSelection}
import relationalClustering.parameterLearning.LearnWeightsLPSupervised
import relationalClustering.representation.clustering.Clustering
import relationalClustering.representation.definition.DefinitionMiner
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity._
import relationalClustering.similarity.kernels.RKOHKernel
import relationalClustering.utils.{Helper, PredicateDeclarations, PrintNTrees}

/**
  * Created by seb on 05.02.16.
  */
object CommandLineInterface {

  //parser specification
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("RelationalClustering.jar", preUsage = Some("Version 2.1"))
  val dbs: MultiValueOption[String] = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
  val head: SingleValueOption[String] = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val declarationFile: SingleValueOption[String] = parser.option[String](List("declarations"), "file path", "file containing declarations of predicates")
  val depth: SingleValueOption[Int] = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val rootFolder: SingleValueOption[String] = parser.option[String](List("root"), "filePath", "folder to place files in")
  val k: SingleValueOption[String] = parser.option[String](List("k"), "n", "number of clusters to create")
  val query: SingleValueOption[String] = parser.option[String](List("query"), "comma-separated list", "list of query domains")
  val weights: SingleValueOption[String] = parser.option[String](List("weights"), "Array[Double]", "comma-separated list of weights [attributes,attribute distribution,connections,vertex neighbourhood, edge distribution]")
  val algorithm: SingleValueOption[String] = parser.option[String](List("algorithm"), "[Spectral|Hierarchical|DBscan|Affinity]", "algorithm to perform clustering")
  val similarity: SingleValueOption[String] = parser.option[String](List("similarity"), "[RCNT|RCNTv2|RCNTnoId|HS|RIBL|HSAG|CCFonseca]", "similarity measure")
  val bag: SingleValueOption[String] = parser.option[String](List("bagSimilarity"), "[chiSquared|maximum|minimum|union]", "bag similarity measure")
  val bagCombination: SingleValueOption[String] = parser.option[String](List("bagCombination"), "[union|intersection]", "bag combination method")
  val linkage: SingleValueOption[String] = parser.option[String](List("linkage"), "[average|complete|ward]", "linkage for hierarchical clustering")
  val validate = parser.flag[Boolean](List("validate"), "should validation be performed")
  val labels = parser.multiOption[String](List("labels"), "file path to the labels", "labels for the query objects")
  val valMethod = parser.option[String](List("validationMethod"), "[ARI|RI|intraCluster|majorityClass]", "cluster validation method")
  val useLocalRepository = parser.flag[Boolean](List("localRepo"), "should NodeRepository be constructed locally for each NeighbourhoodGraph, or one globally shared")
  val clauseLength = parser.option[Int](List("clauseLength"), "n", "maximal length of clause for CCFonseca and RKOH kernel")
  val exportNTs = parser.flag[Boolean](List("exportNTrees"), "should neighbourhood trees be exported as gspan")
  val DBscanEps = parser.option[Double](List("eps"), "d", "eps value for DBscan")
  val AffPreference = parser.option[Double](List("preference"), "d", "preference parameter for Affinity Propagation")
  val AffDamping = parser.option[Double](List("damping"), "d", "damping parameter for Affinity Propagation")
  val aggregatorFunctions = parser.option[String](List("aggregates"), "comma-separated list", "a list of aggregator functions to use for the numerical attributes [mean/min/max] ")
  val learnWeights = parser.flag[Boolean](List("learnWeights"), "learn weights from labels")
  val constraintsFile = parser.option[String](List("constraints"), "filename", "a file containing the constraints for weight learning")
  val numConstraints = parser.option[Int](List("constraintsNumToSample"), "n", "number of constraints to sample - per constraint class")
  val wlRuns = parser.option[Int](List("wlRuns"), "n", "number of weight learning runs")
  val select = parser.flag[Boolean](List("selectSingle"), "flag single clustering")
  val selection = parser.option[String](List("selection"), "method to choose a single clustering", "[model|saturation]")
  val selectionValidation = parser.option[String](List("selectionValidation"), "evaluation criteria for clustering selection", "[intraCluster|silhouette]")
  val edgeCombination = parser.option[String](List("vertexCombination"), "[avg|min|max]", "how to combine values of vertex similarities in hyperedge?")
  val definitions = parser.flag[Boolean](List("findDefinitions"), "extract definitions of clusters")

  def main(args: Array[String]) {
    parser.parse(args)
    require(head.hasValue, "no header specified")
    require(dbs.hasValue, "no databases specified")
    require(query.hasValue, "query not specified")
    require(declarationFile.hasValue, "declarations of the predicates not provided")

    val predicateDeclarations = new PredicateDeclarations(declarationFile.value.get)
    val KnowledgeBase = new KnowledgeBase(dbs.value, Helper.readFile(head.value.get).mkString("\n"), predicateDeclarations)


    val bagComparison = bag.value.getOrElse("chiSquared") match {
      case "chiSquared" => new ChiSquaredDistance()
      case "minimum" => new MinimumSimilarity()
      case "maximum" => new MaximumSimilarity()
      case "union" => new UnionBagSimilarity()
    }

    val bagCombinationMethod = bagCombination.value.getOrElse("intersection") match {
      case "union" => new UnionCombination()
      case "intersection" => new IntersectionCombination()
    }

    val agregates = aggregatorFunctions.value.getOrElse("mean").split(",").toList.foldLeft(List[AbstractAggregator]())( (acc, ag) => {
      ag match {
        case "mean" => acc :+ new AvgAggregator
        case "min" => acc :+ new MinAggregator
        case "max" => acc :+ new MaxAggregator
        case "sum" => acc :+ new SumAggregator
      }
    })




    val weightsToUse = learnWeights.value.getOrElse(false) match {
      case false =>
        //use provided weights
        weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(",").toList.map(_.toDouble)
      case true =>
        //learn the weights
        require(query.value.get.split(",").length == 1, s"When learning the weights, only one domain is supported!")
        require(labels.hasValue, s"No labels provided for weight learning!")

        val labContainer = new LabelsContainer(labels.value)
        val optimizer = new LearnWeightsLPSupervised(labContainer, KnowledgeBase, query.value.get, depth.value.getOrElse(0), bagComparison, bagCombinationMethod, agregates)
        val optimalPars = optimizer.learn()
        println(s"Learned weights: $optimalPars")
        optimalPars
    }




    val similarityMeasure = similarity.value.getOrElse("RCNT") match {
      case "RCNT" =>
        new SimilarityNeighbourhoodTrees(KnowledgeBase,
                                         depth.value.getOrElse(0),
          weightsToUse,
                                         bagComparison,
                                         bagCombinationMethod,
                                         agregates,
                                         useLocalRepository.value.getOrElse(false))
      case "RCNTorder" =>
        new SimilarityNeighbourhoodTreesOrdered(KnowledgeBase,
          depth.value.getOrElse(0),
          weightsToUse,
          bagComparison,
          edgeCombination.value.getOrElse("avg"),
          agregates,
          useLocalRepository.value.getOrElse(false))
      case "HS" => new NevilleSimilarityMeasure(KnowledgeBase)
      case "HSAG" => new HSAG(KnowledgeBase, depth.value.getOrElse(0), bagComparison)
      case "CCFonseca" => new ConceptualFonseca(KnowledgeBase, clauseLength.value.getOrElse(2))
      case "RKOH" => new RKOHKernel(KnowledgeBase, depth.value.getOrElse(0), clauseLength.value.getOrElse(2))
    }



    if (exportNTs.value.getOrElse(false)) {
      println(s"Printing trees to ${rootFolder.value.getOrElse("./tmp")}/gspan...")
      PrintNTrees.saveAll(query.value.get.split(",").toList.head, KnowledgeBase, depth.value.getOrElse(0), rootFolder.value.getOrElse("./tmp"))
    }
    else {

      val clusteringAlg = algorithm.value.getOrElse("Spectral") match {
        case "Spectral" =>
          new Spectral(rootFolder.value.getOrElse("./tmp"))
        case "Hierarchical" =>
          new Hierarchical(linkage.value.getOrElse("average"), rootFolder.value.getOrElse("./tmp"))
        case "DBscan" =>
          new DBScan(DBscanEps.value.getOrElse(0.3), rootFolder.value.getOrElse("./tmp"))
        case "Affinity" =>
          new AffinityPropagation(rootFolder.value.getOrElse("./tmp"), AffDamping.value.getOrElse(0.5), AffPreference.value.getOrElse(0.1))
      }

      val kToUse = k.value.getOrElse("2").contains(":") match {
        case false => List(k.value.getOrElse("2").toInt)
        case true =>
          val tmp = k.value.get.split(":")
          (tmp.head.toInt to tmp.last.toInt).toList
      }

      if (select.value.getOrElse(false)) {
        // IF A SINGLE CLUSTERING SHOULD BE SELECTED
        val clusterings = kToUse.foldLeft(List[Clustering]())((acc, numClust) => acc :+ clusteringAlg.clusterVertices(query.value.get.split(",").toList, similarityMeasure, numClust, 0))

        val validationMethod = selectionValidation.value.getOrElse("intraCluster") match {
          case "intraCluster" =>
            new AverageIntraClusterSimilarity()
          case "silhouette" =>
            new SilhouetteScore(rootFolder.value.getOrElse("./tmp"))
        }

        val selectionMethod = selection.value.getOrElse("saturation") match {
          case "saturation" =>
            new IncreaseSaturationCut(validationMethod, 0.0)
          case "model" =>
            new ModelBasedSelection(validationMethod)
        }

        val selectedCluster = selectionMethod.selectFromClusters(clusterings)

        println(s"FOUND CLUSTERS with k=${selectedCluster.size}")
        selectedCluster.getClusters.zipWithIndex.foreach(cluster => println(s"CLUSTER ${cluster._2}: ${cluster._1.getInstances.map(inst => inst.mkString(":")).mkString(",")}"))

        //if validation is to be performed
        if (validate.value.getOrElse(false)) {
          val labContainer = new LabelsContainer(labels.value)
          valMethod.value.getOrElse("ARI") match {
            case "ARI" =>
              val validator = new AdjustedRandIndex(rootFolder.value.getOrElse("./tmp"))
              println(s"${valMethod.value.get} score: ${validator.validate(selectedCluster, labContainer)}")
            case "intraCluster" =>
              val validator = new AverageIntraClusterSimilarity()
              println(s"${valMethod.value.get} score: ${validator.validate(selectedCluster)}")
            case "majorityClass" =>
              val validator = new MajorityClass()
              println(s"${valMethod.value.get} score: ${validator.validate(selectedCluster, labContainer)}")
            case "ANMI" =>
              val validator = new AdjustedNMI(rootFolder.value.getOrElse("./tmp"))
              println(s"${valMethod.value.get} score: ${validator.validate(selectedCluster, labContainer)}")
          }
        }

        println("*" * 30)
        if (definitions.value.getOrElse(false)) {
          val miner = new DefinitionMiner(selectedCluster)
          val defs = miner.getDefinitions(weightsToUse)
          defs.foreach(cdef => {
            println(s"${cdef._1}\n\n" + cdef._2.map(_.toString).mkString("\n"))
          })
        }

      }
      else {
        //IF ONLY CLUSTERING IS TO BE PERFORMED
        kToUse.foreach(numClust => {
          val clustering = clusteringAlg.clusterVertices(query.value.get.split(",").toList, similarityMeasure, numClust, 0)


          println(s"FOUND CLUSTERS with k=$numClust")
          clustering.getClusters.zipWithIndex.foreach(cluster => println(s"CLUSTER ${cluster._2}: ${cluster._1.getInstances.map(inst => inst.mkString(":")).mkString(",")}"))


          //if validation is to be performed
          if (validate.value.getOrElse(false)) {
            val labContainer = new LabelsContainer(labels.value)
            valMethod.value.getOrElse("ARI") match {
              case "ARI" =>
                val validator = new AdjustedRandIndex(rootFolder.value.getOrElse("./tmp"))
                println(s"${valMethod.value.getOrElse("ARI")} score: ${validator.validate(clustering, labContainer)}")
              case "intraCluster" =>
                val validator = new AverageIntraClusterSimilarity()
                println(s"${valMethod.value.getOrElse("Intra cluster similarity")} score: ${validator.validate(clustering)}")
              case "majorityClass" =>
                val validator = new MajorityClass()
                println(s"${valMethod.value.get} score: ${validator.validate(clustering, labContainer)}")
            }
          }

          println("*" * 30)

          if (definitions.value.getOrElse(false)) {
            val miner = new DefinitionMiner(clustering)
            val defs = miner.getDefinitions(weightsToUse)
            defs.foreach(cdef => {
              println(s"${cdef._1}\n\n" + cdef._2.map(_.toString).mkString("\n"))
            })
          }
        })

      }




    }
  }
}
