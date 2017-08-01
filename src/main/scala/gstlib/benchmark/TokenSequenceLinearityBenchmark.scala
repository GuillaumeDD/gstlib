/*
 * Copyright LIMSI-CNRS
 *
 * Contributor(s) :
 *    Guillaume Dubuisson Duplessis <gdubuisson@limsi.fr> (2016-2017)
 *
 * This software is a computer program whose purpose is to implement a
 * generalized suffix tree datastructure and associated algorithms for
 * sequences of items in the Scala programming language.
 *
 * This software is governed by the CeCILL-B license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL-B
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL-B license and that you accept its terms.
 *
 */
package gstlib.benchmark

import gstlib.GeneralizedSuffixTreeBuilder

import scala.io.Source
import org.scalameter._

/**
  * Benchmark that checks the time linearity of the building of a generalized
  * suffix tree based on sequence of tokens.
  *
  * @author Guillaume Dubuisson Duplessis
  */
object TokenSequenceLinearityBenchmark extends App {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 5,
    Key.verbose -> false
  ) withWarmer (new Warmer.Default)

  type Token = String
  type TokenizedUtterance = Array[String]

  def buildStringSuffixTree() =
    GeneralizedSuffixTreeBuilder.empty[Char, Array[Char]]()

  val filenames = List(
    "pg21782A.txt", "pg21782B.txt", "pg21782C.txt", "pg21782D.txt",
    "pg21782E.txt", "pg21782F.txt", "pg21782G.txt",
    "pg0766A.txt", "pg0766B.txt",
    "pg0766C.txt",
    "pg0766D.txt")

  println(s"Tokens\tTime")
  for (filename <- filenames) {
    val sourceDir = Source.fromURL(getClass.getResource(s"/benchmark/$filename"))
    val content = sourceDir.getLines().mkString

    val lines = content.split("\\s+") // tokenization

    val size = lines.size // size in number of tokens

    val time = standardConfig measure {
      val stree = GeneralizedSuffixTreeBuilder.empty[Token, TokenizedUtterance]()
      stree.insert(lines)
    }
    println(s"$size\t$time")
  }

}