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
package gstlib

import org.scalatest.FunSuite

/**
  * @author Guillaume Dubuisson Duplessis
  */
class GeneralizedSuffixTreeBuilderTest extends FunSuite {


  test("Builder method: +=, result and clear") {
    val streeBuilder = GeneralizedSuffixTree.newBuilder[Char, String]

    // Empty builder
    val stree1 = streeBuilder.result()
    assert(stree1.size() == 0)
    assert(stree1.nSuffixes() == 0)

    streeBuilder.clear()

    // Non-empty builder
    streeBuilder += "hello"
    streeBuilder += "world"
    streeBuilder += "!"
    val stree2 = streeBuilder.result()
    assert(stree2.size() == 3)
    assert(stree2.getSequenceBy(0) == "hello")
    assert(stree2.getSequenceBy(1) == "world")
    assert(stree2.getSequenceBy(2) == "!")

    assert(stree2.find("orl") == List((1, 1)))

    // Clearing builder
    streeBuilder.clear()
    streeBuilder += "foo"
    streeBuilder += "bar"
    val stree3 = streeBuilder.result()
    assert(stree3.size() == 2)
    assert(stree3.getSequenceBy(0) == "foo")
    assert(stree3.getSequenceBy(1) == "bar")

    assert(stree3.find("orl") == List())
    assert(stree3.find("r") == List((1, 2)))
  }

  test("Builder method: ++=") {
    val streeBuilder = GeneralizedSuffixTree.newBuilder[String, List[String]]

    val utterances = List(
      List("foo", "bar"),
      List("hello", "world", "!"),
      List("a", "nice", "library", ".")
    )

    streeBuilder ++= utterances.toIterator
    val stree = streeBuilder.result()
    assert(stree.size == 3)
    assert(stree.getSequenceBy(0) == List("foo", "bar"))
    assert(stree.getSequenceBy(1) == List("hello", "world", "!"))
    assert(stree.getSequenceBy(2) == List("a", "nice", "library", "."))
  }
}