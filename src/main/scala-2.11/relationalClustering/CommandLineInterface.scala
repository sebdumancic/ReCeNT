package relationalClustering

import org.clapper.argot.ArgotParser
import relationalClustering.aggregators._
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, UnionBagSimilarity}
import relationalClustering.clustering.evaluation.{AdjustedRandIndex, AverageIntraClusterSimilarity, LabelsContainer, MajorityClass}
import relationalClustering.clustering.{AffinityPropagation, DBScan, Hierarchical, Spectral}
import relationalClustering.parameterLearning.{ConstraintsContainer, LearnWeightsLP, SampleConstraints}
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
  val dbs = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
  val head = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val declarationFile = parser.option[String](List("declarations"), "file path", "file containing declarations of predicates")
  val depth = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val rootFolder = parser.option[String](List("root"), "filePath", "folder to place files in")
  val k = parser.option[Int](List("k"), "n", "number of clusters to create")
  val query = parser.option[String](List("query"), "comma-separated list", "list of query domains")
  val weights = parser.option[String](List("weights"), "Array[Double]", "comma-separated list of weights [attributes,attribute distribution,connections,vertex neighbourhood, edge distribution]")
  val algorithm = parser.option[String](List("algorithm"), "[Spectral|Hierarchical|DBscan|Affinity]", "algorithm to perform clustering")
  val similarity = parser.option[String](List("similarity"), "[RCNT|RCNTv2|RCNTnoId|HS|RIBL|HSAG|CCFonseca]", "similarity measure")
  val bag = parser.option[String](List("bagSimilarity"), "[chiSquared|maximum|minimum|union]", "bag similarity measure")
  val bagCombination = parser.option[String](List("bagCombination"), "[union|intersection]", "bag combination method")
  val linkage = parser.option[String](List("linkage"), "[average|complete|ward]", "linkage for hierarchical clustering")
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
  val learnWeights = parser.flag[Boolean](List("learnWeights"), "learn weights from constraints")
  val constraintsFile = parser.option[String](List("constraints"), "filename", "a file containing the constraints for weight learning")
  val numConstraints = parser.option[Int](List("constraintsNumToSample"), "n", "number of constraints to sample - per constraint class")


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
        weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(",").toList.map(_.toDouble)
      case true =>
        require(query.value.get.split(",").length == 1, s"When learning the weights, only one domain is supported!")
        require(constraintsFile.hasValue, s"No constraints provided for weight learning!")
        val constraintsCon = constraintsFile.value.getOrElse("") == "" match {
          case false => new ConstraintsContainer(constraintsFile.value.get, query.value.get, KnowledgeBase, depth.value.getOrElse(0))
          case true =>
            val labContainer = new LabelsContainer(labels.value)
            val constraintsSampler = new SampleConstraints(labContainer, KnowledgeBase.getDomain(query.value.get), KnowledgeBase, depth.value.getOrElse(0))
            constraintsSampler.sample(numConstraints.value.getOrElse(20))
        }
        val optimizer = new LearnWeightsLP(constraintsCon, KnowledgeBase, depth.value.getOrElse(0), bagComparison, bagCombinationMethod, agregates)
        val optimalPars = optimizer.learn()
        println(s"Found optimal parameters: $optimalPars")
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
      case "RCNTv2" =>
        new SimilarityNTv2(KnowledgeBase,
                           depth.value.getOrElse(0),
          weightsToUse,
                           bagComparison,
                           bagCombinationMethod,
                           agregates,
                           useLocalRepository.value.getOrElse(false))
      case "RCNTnoId" =>
        new SimilarityNTNoIdentities(KnowledgeBase,
                                     depth.value.getOrElse(0),
          weightsToUse,
                                     bagComparison,
                                     bagCombinationMethod,
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

      val clustering = clusteringAlg.clusterVertices(query.value.get.split(",").toList, similarityMeasure, k.value.getOrElse(2), 0)

      println("FOUND CLUSTERS")
      clustering.getClusters.zipWithIndex.foreach(cluster => println(s"CLUSTER ${cluster._2}: ${cluster._1.getInstances.map( inst => inst.mkString(":")).mkString(",")}"))


      //if validation is to be performed
      if (validate.value.getOrElse(false)) {
        val labContainer = new LabelsContainer(labels.value)
        valMethod.value.getOrElse("ARI") match {
          case "ARI" =>
            val validator = new AdjustedRandIndex(rootFolder.value.getOrElse("./tmp"))
            println(s"${valMethod.value.getOrElse("ARI")} score: ${validator.validate(clustering, labContainer)}")
          case "intraCluster" =>
            val validator = new AverageIntraClusterSimilarity()
            println(s"${valMethod.value.getOrElse("ARI")} score: ${validator.validate(clustering)}")
          case "majorityClass" =>
            val validator = new MajorityClass()
            println(s"${valMethod.value.get} score: ${validator.validate(clustering, labContainer)}")
        }

      }
    }
  }
}
