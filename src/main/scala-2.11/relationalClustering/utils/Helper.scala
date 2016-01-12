package relationalClustering.utils

import java.io.FileWriter

import scala.io.Source

/**
 *
 * A helper object containing small functions that does not belong to any specific objects
 * Created by seb on 29.06.15.
 */
object Helper {

  def bool2int(value: Boolean): Int = {
    if (value) 1 else 0
  }

  def readFile(filename: String) = {
    val fileSource = Source.fromFile(filename)
    val lines = fileSource.getLines().mkString("\n")
    fileSource.close()
    lines.split("\n")
  }

}
