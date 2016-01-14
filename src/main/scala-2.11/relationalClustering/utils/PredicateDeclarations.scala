package relationalClustering.utils

/**
  * Created by seb on 13.01.16.
  */
class PredicateDeclarations(val filename: String) {

  private val declarations = collection.mutable.Map[String, List[String]]()
  private val possibleDeclarations = List[String]("attribute", "name", "number")
  processFile()


  def getFilename = { filename }
  def addDeclaration(predicate: String, types: List[String]) = {
    require(types.map(possibleDeclarations.contains(_)).reduce( _ && _), s"Unknown declaration $types; possible declarations $possibleDeclarations")
    declarations(predicate) = types
  }


  private def processFile() = {
    val declarationRegex = """(.*)\((.*)\)""".r

    Helper.readFile(getFilename).filter( x => !(x.length < 3 || x.startsWith("//"))).foreach( line => {
      val declarationRegex(predicate, args) = line
        addDeclaration(predicate, args.split(",").toList)
    })
  }

  def getArgumentType(predicate: String, position: Int) = { declarations(predicate)(position) }
}
