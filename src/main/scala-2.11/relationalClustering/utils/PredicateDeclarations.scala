package relationalClustering.utils

/**
  * Created by seb on 13.01.16.
  */
class PredicateDeclarations(val filename: String) {

  private val declarations = collection.mutable.Map[String, List[String]]()
  processFile()


  def getFilename = { filename }
  def addDeclaration(predicate: String, types: List[String]) = {
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
