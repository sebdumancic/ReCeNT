package relationalClustering.utils

/**
  * Structure to store declarations of the predicate's arguments
  *
  * @constructor reads the declarations of the predicates
  * @param filename file containing the declarations of the predicates: [[String]]
  * Created by seb on 13.01.16.
  */
class PredicateDeclarations(val filename: String) {

  private val declarations = collection.mutable.Map[String, List[String]]()
  private val possibleDeclarations = List[String](Settings.ARG_TYPE_ATTRIBUTE, Settings.ARG_TYPE_NAME, Settings.ARG_TYPE_NUMBER)
  processFile()

  /** Returns the filename of the declaration*/
  def getFilename = { filename }

  /** Adds the declarations to the predicate
    *
    * @param predicate name of the predicate: [[String]]
    * @param types arguments types: [[List]]
    * */
  def addDeclaration(predicate: String, types: List[String]) = {
    require(types.map(possibleDeclarations.contains(_)).reduce( _ && _), s"Unknown declaration $types; possible declarations $possibleDeclarations")
    declarations(predicate) = types
  }

  /** Reads the declarations file */
  private def processFile() = {
    val declarationRegex = """(.*)\((.*)\)""".r

    Helper.readFile(getFilename).filter( x => !(x.length < 3 || x.startsWith("//"))).foreach( line => {
      val declarationRegex(predicate, args) = line
        addDeclaration(predicate, args.split(",").toList)
    })
  }

  /** Returns the role of the specified position
    *
    * @param predicate name of the predicate: [[String]]
    * @param position position of the arguments: [[Int]]
    * */
  def getArgumentType(predicate: String, position: Int) = {
    declarations(predicate)(position)
  }

  /** Returns the list of all argument type of the predicate
    *
    * @param predicate name of the predicate: [[String]]
    * @return [[List]] of types
    * */
  def getArgumentTypes(predicate: String) = {
    declarations(predicate)
  }

  /** Returns the role of the predicate
    *
    * @param predicate name of the predicate: [[String]]
    * @return [[String]]
    * */
  def getPredicateRole(predicate: String) = {
    val predicateArgsDeclaration = declarations(predicate)

    if (predicateArgsDeclaration.length == 1) { Settings.ROLE_ANNOTATION }
    else if (predicateArgsDeclaration.contains(Settings.ARG_TYPE_ATTRIBUTE) || predicateArgsDeclaration.contains(Settings.ARG_TYPE_NUMBER)) { Settings.ROLE_ATTRIBUTE }
    else { Settings.ROLE_HYPEREDGE }
  }
}
