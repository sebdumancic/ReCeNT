package relationalClustering.representation

/**
 *  This class implements predicate functionality for finding candidates to combine.
 *  It stores the basic information about the predicate, as well as true groundings
  *
  * @constructor creates a predicate with given name and domains
  * @param name name of the predicate: String
  * @param domains domains on which the predicate operates: List[Domain]
 *
 * Created by seb on 08.05.15.
 */
class Predicate(
               protected var name: String,
               protected val domains: List[Domain],
               protected val role: String
                 ) {

  protected val trueGroundings: collection.mutable.Set[List[String]] = collection.mutable.Set[List[String]]()
  protected var argumentDeclarations = List[String]()

  /** Returns the set of true groundings
    * @return Set[List[String] ]*/
  def getTrueGroundings: collection.mutable.Set[List[String]] = { trueGroundings }

  /** Sets the true assignment of the predicate for the given arguments
    *
    * @param ground list of arguments: List[String]
    * */
  def setTrueGrounding(ground: List[String]) = {
    require( ground.length == arity, s"Wrong number of arguments [$name($ground)] -- ${ground.length} != $arity")
    trueGroundings += ground

    //add each elements to its domain
    ground.zipWithIndex.foreach( elem => domains(elem._2).addElement(elem._1))
  }

  /** Sets the false assignment of the predicate fpr the given arguments
    *
    * @param ground list of arguments: List[String]
    * */
  def removeTrueGrounding(ground: List[String]) = {
    require( ground.size == arity, "Wrong number of arguments")
    trueGroundings -= ground
  }

  /** Returns the arity of the predicate
    *
    * @return Int
    * */
  def arity: Int = {
    domains.length
  }

  /** Returns the name of the predicate
    * @return name: String
    * */
  def getName: String = {
    name
  }

  /** Returns the domains of the predicate
    *
    * @return List[String]
    * */
  def getDomains: List[String] = {
    domains.map( _.getName )
  }

  /** Returns the Domains objects of the predicate
    * @return List[Domain]
    * */
  def getDomainObjects = {
    domains
  }

  /** Checks whether given arguments evaluates to true
    *
    * @param grounding list of arguments: List[String]
    * @return Boolean
    * */
  def isTrue(grounding: List[String]) = {
    trueGroundings.contains(grounding)
  }

  override def toString = getName + "(" + getDomains.mkString(",") + ")"

  /** Sets a new name for the predicate
    *
    * @param newName new name to be set: String
    * */
  def setName(newName: String) = {
    name = newName
  }

  /** Sets the declarations of the arguments */
  def setDeclarations(declarations: List[String]) = {
    argumentDeclarations = declarations
  }

  /** Returns the role of the argument at the specified position
    *
    * @param position position of the arguments: Int
    * */
  def getArgumentRole(position: Int) = { argumentDeclarations(position) }

  /** Returns the roles of the arguments
    * */
  def getArgumentRoles = {
    argumentDeclarations
  }

  /** Returns the role of the predicate*/
  def getRole = {
    role
  }
}
