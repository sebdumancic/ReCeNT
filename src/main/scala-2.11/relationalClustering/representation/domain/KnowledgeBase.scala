package relationalClustering.representation.domain

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.utils.{Helper, PredicateDeclarations}


/**
 *  This class implements a knowledge base managing predicates and their true groundings
 *
  * @constructor creates a knowledge base from the set of facts, definitions and declarations of the predicates
  * @param databases a list of databases containing logical facts: Set[String]
  * @param header definitions of the predicates: String
  * @param predicateDeclarations declarations of the predicates: [[PredicateDeclarations]]
 * Created by seb on 08.05.15.
 */


class KnowledgeBase(private val databases: Seq[String],
                   private val header: String,
                   private val predicateDeclarations: PredicateDeclarations
                   ) {

  private val predicates = collection.mutable.Map[String, Predicate]()
  private val predicateRegex = """(.*?)\((.*?)\).*""".r
  private val domainObjects = collection.mutable.Map[String, Domain]()

  constructPredicates()
  processDatabases()

  /** Returns the domain with speficified name
    *
    * @param name name of the domain: [[String]]
    * @return Domain: [[Domain]]
    * */
  def getDomainObject(name: String) = {
    if (!domainObjects.contains(name)) { domainObjects(name) = new Domain(name)}
    domainObjects(name)
  }

  /** Creates a predicate with the given name and domains
    *
    * @param name name of the predicate: [[String]]
    * @param domains list of domain a predicate operates on: [[List]]
    * */
  def createPredicate(name: String, domains: List[String]) = {
    predicates(name) = new Predicate(name.trim, domains.map( x => getDomainObject(x.trim) ), predicateDeclarations.getPredicateRole(name.trim))
    predicates(name).setDeclarations(predicateDeclarations.getArgumentTypes(name))
  }

  /** Adds predicate to the collection of predicate
    *
    * @param pred predicate to be added: [[Predicate]]
    * */
  def addPredicate(pred: Predicate) = {
    predicates(pred.getName) = pred
  }

  /** Returns a predicate with a given name
    *
    * @param name name of the predicate: [[String]]
    * @return [[Predicate]]
    * */
  def getPredicate(name: String) = {
    require(predicates contains name, s"Requesting non-existing predicate $name!")
    predicates(name)
  }

  /** Returns the list of all predicate names
    *
    * @return [[List]]
    * */
  def getPredicateNames = {
    predicates.keys.toList
  }

  /** Adds an object to the domain
    *
    * @param domain name of the domain: [[String]]
    * @param domainObject objects name: [[String]]
    * */
  def addToDomain(domain: String, domainObject: String) = {
    getDomainObject(domain.trim).addElement(domainObject)
  }

  /** Returns the domain with a specified name
    *
    * @param domain name of the domain: [[String]]
    * @return [[Domain]]
    * */
  def getDomain(domain: String) = {
    require(domainObjects contains domain, s"Accessing a non-existing domain $domain")
    domainObjects(domain)
  }

  /** Returns the set of all domain objects*/
  def getAllDomains = { domainObjects }

  /** Constructs the predicates specified in the header*/
  private def constructPredicates() = {
    for(predicate <- header.split("\n")) {
      if (predicate.length > 2 && !predicate.contains("//")) {
        val predicateRegex(predicate_name, predicate_args) = predicate
        createPredicate(cleanForm(predicate_name), predicate_args.split(",").toList) // cleans predicate name in case it is in the autoencoder form
      }

    }
  }

  /** Adds a true grounding
    *
    * @param grounding a grounding of the predicate predicate(arguments): [[String]]
    *
    * Each encountered object is also added to its domain
    * */
  def addTrueGrounding(grounding: String) = {
    val predicateRegex(predicate_name, predicate_arguments) = grounding
    val argument_list = predicate_arguments.split(",").toList
    getPredicate(predicate_name).setTrueGrounding(argument_list.map( _.trim ))

    // add an element to the domain
    for (mapping <- argument_list.zip(getPredicate(predicate_name).getDomains)) {
      addToDomain(mapping._2, mapping._1.trim)
    }
  }

  private def cleanForm(line: String) = {
    //in PredicateInvention a model have to be re-instantiated with a new knowledge bases that might be in form of predicateName[_source|_target]
    line.replace("_target", "").replace("_source", "")
  }

  /** Read a specified database
    *
    * @param db a filepath to the database to read: [[String]]
    * */
  private def processDatabase(db: String) = {
    for(line <- Helper.readFile(db)) {
      if (line.length > 2) {
        addTrueGrounding(cleanForm(line))
      }
    }
  }

  /** Reads all specified databases */
  private def processDatabases() = {
    for(db <- databases) {
      processDatabase(db)
    }
  }

  def printToFile(filename: String) = {
    val writer = new BufferedWriter(new FileWriter(filename))

    for(predicate <- predicates.keys) {
      for(grounding <- predicates(predicate).getTrueGroundings) {
        writer.write(s"$predicate(" + grounding.mkString(",") + ").\n")
      }
    }
    writer.close()
  }


  def printToFileWithTarget(filename: String, target: String) = {

    val writer = new BufferedWriter(new FileWriter(filename))

    // generate everything except target predicate
    for(predicate <- getPredicateNames) {
      if (predicate != target) {
        for (grounding <- predicates(predicate).getTrueGroundings) {
          writer.write(predicate.toLowerCase.replace('-','_') + "(" + grounding.mkString(",").toLowerCase + ").\n")
        }
      }
    }

    // add target predicate with truth values
    val target_predicate = getPredicate(target)
    var domains = collection.mutable.Seq[collection.mutable.Set[List[String]]]()

    for (dom <- target_predicate.getDomains) {
      domains = domains :+ getDomain(dom).getElementsAsList
    }

    for (ground <- domains.reduceLeft( (x,y) => { for { xs <-x; ys <- y} yield xs ::: ys} )) {
      if (target_predicate.isTrue(ground)) {
        writer.write(target.toLowerCase.replace('-','_') + "(" + ground.mkString(",").toLowerCase + ",pos).\n" )
      }
      else {
        writer.write(target.toLowerCase.replace('-','_') + "(" + ground.mkString(",").toLowerCase + ",neg).\n" )
      }
    }

    writer.close()
  }

}


