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

import GeneralizedSuffixTree._
import org.scalatest.FunSuite

import scala.io.Source

/**
  * @author Guillaume Dubuisson Duplessis
  */
class GeneralizedSuffixTreeTest extends FunSuite {

  trait BuiltTree {
    val builder = GeneralizedSuffixTree.newBuilder[Char, String]

    val content = List("axa", "axabxb", "axabxb", "mississippi", "banana", "abcdefghijklmnopqrstuvwxyz")
    builder ++= content

    val stree = builder.result()

    val nbSuffixes = content.map(_.length).sum
    val suffixes = List("a", "xa", "axa",
      "b", "xb", "bxb", "abxb", "xabxb", "axabxb",
      "i", "pi", "ppi", "ippi", "sippi", "ssippi", "issippi", "sissippi", "ssissippi", "ississippi", "mississippi",
      "a", "na", "ana", "nana", "anana", "banana",
      "z", "yz", "xyz", "wxyz", "vwxyz", "uvwxyz", "tuvwxyz", "stuvwxyz", "rstuvwxyz", "qrstuvwxyz", "pqrstuvwxyz",
      "opqrstuvwxyz", "nopqrstuvwxyz", "mnopqrstuvwxyz", "lmnopqrstuvwxyz", "klmnopqrstuvwxyz", "jklmnopqrstuvwxyz",
      "ijklmnopqrstuvwxyz", "hijklmnopqrstuvwxyz", "ghijklmnopqrstuvwxyz", "fghijklmnopqrstuvwxyz", "efghijklmnopqrstuvwxyz",
      "defghijklmnopqrstuvwxyz", "cdefghijklmnopqrstuvwxyz", "bcdefghijklmnopqrstuvwxyz", "abcdefghijklmnopqrstuvwxyz")

  }

  test("contains") {
    new BuiltTree {
      assert(suffixes.forall(stree.contains(_)))

      assert(!stree.contains("test"))
    }
  }

  test("find") {
    new BuiltTree {
      val solutions01 = List((0, 2), (0, 0), (1, 0), (1, 2), (2, 0), (2, 2), (4, 1), (4, 3), (4, 5), (5, 0))
      val testSolutions01 = stree.find("a")
      assert(testSolutions01.size == solutions01.size)
      assert(solutions01.forall(testSolutions01.contains(_)))

      val solutions02 = List((1, 2), (2, 2), (5, 0))
      val testSolutions02 = stree.find("ab")
      assert(testSolutions02.size == solutions02.size)
      assert(solutions02.forall(testSolutions02.contains(_)))
    }
  }

  test("findLongestCommonSubsequence") {
    new BuiltTree {
      // Inexisting sequence
      val pattern01 = "123456789"
      assert(stree.findLongestCommonSubsequences(pattern01).isEmpty)

      // Existing sub-sequence
      val pattern02 = "orange"
      val positions02 = Set((4, 1), (4, 3))
      // positions in 'banana'
      val results02 = stree.findLongestCommonSubsequences(pattern02)
      assert(results02.size == 1)
      val (start02, end02, computedPositions02) = results02.head
      assert(start02 == 2)
      assert(end02 == 4)
      assert(computedPositions02.size == positions02.size)
      assert(positions02 == computedPositions02)

      // Existing several sub-sequences: "ax", "xa" and "an"
      val pattern02b = "ax xa an z"
      // "ax" [0;1] is in 3 sequences: 0 (at pos 0), 1 (at pos 0) and 2 (at pos 0)
      val solutions02b_ax = (0, 2, Set((0, 0), (1, 0), (2, 0)))
      // "xa" [3;4] is in 3 sequences: 0 (at pos 1), 1 (at pos 1) and 2 (at pos 1)
      val solutions02b_xa = (3, 5, Set((0, 1), (1, 1), (2, 1)))
      // "an" [6;7] is in 1 sequence: 4 (at pos 1 and 3)
      val solutions02b_an = (6, 8, Set((4, 1), (4, 3)))

      val results02b = stree.findLongestCommonSubsequences(pattern02b)
      assert(results02b.size == 3)
      assert(results02b.contains(solutions02b_ax))
      assert(results02b.contains(solutions02b_xa))
      assert(results02b.contains(solutions02b_an))

      // Existing full-sequence
      val pattern03 = "ssissi"
      val positions03 = Set((3, 2))
      // positions in 'mississippi'
      val results03 = stree.findLongestCommonSubsequences(pattern03)
      assert(results03.size == 1)
      val (start03, end03, computedPositions03) = results03.head
      assert(start03 == 0)
      assert(end03 == 6)
      assert(computedPositions03.size == positions03.size)
      assert(positions03 == computedPositions03)

      // Existing full-sequence
      val pattern04 = "i"
      val positions04 = Set((3, 7), (3, 4), (3, 1), (3, 10), // positions in 'mississippi'
        (5, 8) // positions in 'abcdefghijklmnopqrstuvwxyz'
      )
      val results04 = stree.findLongestCommonSubsequences(pattern04)
      assert(results04.size == 1)
      val (start04, end04, computedPositions04) = results04.head
      assert(start04 == 0)
      assert(end04 == 1)
      assert(computedPositions04.size == positions04.size)
      assert(positions04 == computedPositions04)
    }
  }

  test("findLongestCommonSubsequence: documentation example") {
    val stree = GeneralizedSuffixTree[String, Array[String]](
      Array("Scala", "is", "awesome", "!"),
      Array("Programming", "is", "awesome", "."),
      Array("Scala", "is", "cool", "!")
    )


    // Existing several sub-sequences: "ax", "xa" and "an"
    val pattern = Array("Well", ",", "Scala", "is", "...", "cool", "!")
    val solutions1 = (2, 4, Set((0, 0), (2, 0)))
    val solutions2 = (5, 7, Set((2, 2)))

    val results = stree.findLongestCommonSubsequences(pattern)
    assert(results.size == 2)
    assert(results.contains(solutions1))
    assert(results.contains(solutions2))
  }

  test("suffixes") {
    new BuiltTree {
      assert(stree.nSuffixes() == nbSuffixes)
      assert(stree.suffixes.length == nbSuffixes)
      assert(stree.suffixes.forall(suffixes.contains(_)))
    }

    val seq = List("#BEGIN#", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",")
    val suffixesSolution = for {
      start <- 0 until seq.size
    } yield (seq.slice(start, seq.size))

    val stree = GeneralizedSuffixTree[String, List[String]](seq)

    assert(stree.nSuffixes() == suffixesSolution.size)
    assert(stree.suffixes.length == suffixesSolution.size)
    assert(stree.suffixes.forall(suffixesSolution.contains(_)))
  }

  test("fullSequences") {
    new BuiltTree {
      assert(stree.fullSequences.forall(content.contains(_)))
    }
  }

  test("multipleCommonSubsequence") {
    // First test
    val stree = GeneralizedSuffixTree[Char, String]("abcde", "abcde")

    val result = stree.multipleCommonSubsequence()

    assert(result.nb(1) == 0)
    assert(result.nb(2) == 5)
    assert(result.nb(3) == 0)

    assert(result.longest(1) == 0)
    assert(result.longest(2) == 5)
    assert(result.longest(3) == 0)

    assert(result(2).contains("abcde"))
    assert(result(2).contains("bcde"))
    assert(result(2).contains("cde"))
    assert(result(2).contains("de"))
    assert(result(2).contains("e"))

    assert(result(2).toList.map(_.size) == List(5, 4, 3, 2, 1))

    val stree1 =
      GeneralizedSuffixTree[String, List[String]](
        List("Hello", "world", "!"),
        List("Hello", "Bob", "!"),
        List("You", "look", "good", "today"),
        List("You", "are", "very", "good", "today"),
        List("You", "are", "very", "nice", "today"),
        List("I", "was", "nice", "yesterday")
      )

    val result1 = stree1.multipleCommonSubsequence()

    assert(result1.nb(1) == 0)
    assert(result1.nb(2) == 7)
    assert(result1.nb(3) == 2)

    assert(result1.longest(1) == 0)
    assert(result1.longest(2) == 3)
    assert(result1.longest(3) == 1)

    assert(result1(2).contains(List("Hello")))
    assert(result1(2).contains(List("!")))
    assert(result1(2).contains(List("good", "today")))
    assert(result1(2).contains(List("nice")))
    assert(result1(2).contains(List("You", "are", "very")))
    assert(result1(2).contains(List("are", "very")))
    assert(result1(2).contains(List("very")))

    assert(result1(3).contains(List("You")))
    assert(result1(3).contains(List("today")))
  }

  test("bulkMultipleCommonSubsequence") {
    val stree =
      GeneralizedSuffixTree[String, List[String]](
        List("Hello", "world", "!"),
        List("Hello", "Bob", "!"),
        List("You", "look", "good", "today"),
        List("You", "are", "very", "good", "today"),
        List("You", "are", "very", "nice", "today"),
        List("I", "was", "nice", "yesterday"))

    val result = stree.bulkMultipleCommonSubsequence().toList

    assert(result.size == 9,
      s"${result.mkString(", ")}")

    assert(result.contains((2, List("Hello"))))
    assert(result.contains((2, List("!"))))
    assert(result.contains((2, List("good", "today"))))
    assert(result.contains((2, List("nice"))))
    assert(result.contains((2, List("You", "are", "very"))))
    assert(result.contains((2, List("are", "very"))))
    assert(result.contains((2, List("very"))))

    assert(result.contains((3, List("You"))))
    assert(result.contains((3, List("today"))))
  }

  test("MCSP: Multiple Common Subsequence Problem resolution") {
    // Loading the database file that contains utterance
    val database = Source.fromURL(getClass.getResource(s"/database_utterances.txt"))

    val builder = GeneralizedSuffixTree.newBuilder[String, Array[String]]
    try {
      // Loading the database in the tree
      for {
        line <- database.getLines()
      } {
        val utterance = line.trim().split("""\s""").toArray
        builder += utterance
      }

      val stree = builder.result()

      // Computing the MCSP with the 2 methods, and converting Array[String] to List[String]
      // for a simpler comparison of sets
      val resultMCSP1 = stree.multipleCommonSubsequence().toIterator.map(
        {
          case (freq, pattern) => (freq, pattern.toList)
        }).toSet
      val resultMCSP2 = stree.bulkMultipleCommonSubsequence().map(
        {
          case (freq, pattern) => (freq, pattern.toList)
        }).toSet
      // Comparison of the results
      assert(resultMCSP1 == resultMCSP2)

    } catch {
      case e: Exception =>
        fail(e)
    } finally {
      database.close()
    }
  }

  test("longestSubsequence") {
    val sequence01_a = "test"
    val sequence02_a = "lol"
    assert(longestSubsequence(sequence01_a, sequence02_a) == None)

    val sequence01_b = "hello"
    val sequence02_b = "hell no"
    assert(longestSubsequence(sequence01_b, sequence02_b) == Some("hell"))

    val sequence01_c = List("Hi", "my", "name", "is", "Alice")
    val sequence02_c = List("Hello", "my", "name", "is", "Bob")
    assert(longestSubsequence(sequence01_c, sequence02_c) == Some(List("my", "name", "is")))

    val sequence01_d = List("#BEGIN#", "i", "am", "not")
    val sequence02_d = List("#BEGIN#", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",")
    assert(longestSubsequence(sequence01_d, sequence02_d) == Some(List("#BEGIN#")))

    val sequence01_e = List("wanda")
    val sequence02_e = List("#BEGIN#", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",", "ha", ",")
    assert(longestSubsequence(sequence01_e, sequence02_e) == None)
  }
}
