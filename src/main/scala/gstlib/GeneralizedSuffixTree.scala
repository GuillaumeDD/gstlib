/*
 * Copyright LIMSI-CNRS
 *
 * Contributor(s) :
 *    Guillaume Dubuisson Duplessis <gdubuisson@limsi.fr> (2016-2017)
 *    Vincent Letard <vincent.letard@limsi.fr> (2016)
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

import gstlib.GeneralizedSuffixTreeBuilder.{Sequence, SequenceID}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait GeneralizedSuffixTree[Alphabet, Repr] {
  /**
    * Retrieves a sequence by a given ID
    *
    */
  def getSequenceBy(id: SequenceID): Repr

  /**
    * Number of sequences in this suffix tree
    *
    */
  def size(): Int

  /**
    * Computes an iterator over the full sequences of this generalized suffix tree
    *
    */
  def fullSequences(): Iterator[Repr]

  /**
    * Computes an iterator over the suffixes of this generalized suffix tree
    */
  def suffixes(): Iterator[Repr]

  /**
    * Number of suffixes in this generalized suffix tree
    *
    */
  def nSuffixes(): Int

  /**
    * Determines the longest common subsequences between a given pattern and the
    * generalized suffix tree.
    *
    * For instance, given a suffix tree containing sequences:
    *   - Scala|is|awesome|! (sequence 0)
    *   - Programming|is|awesome|. (sequence 1)
    *   - Scala|is|cool|! (sequence 2)
    * And given a pattern: |Well|,|Scala|is|...|cool|!
    *
    * It will return:
    *   - (2, 4, Set((0, 0), (2, 0))) i.e. the position of subpattern Scala|is in sequences 0 and 2
    *   - (5, 7, Set((2, 2))) i.e. the position of subpattern cool|! in sequence 2
    *
    * @return an empty list if the longest common subsequence does not exist,
    *         else a list of triple containing the starting index (inclusive, starting from 0),
    *         the end index (exclusive) and
    *         the set of sequence IDs containing this subsequence along with the starting position
    *         of the (sub)pattern in the sequence (starting from 0).
    *
    */
  def findLongestCommonSubsequences(pattern: Repr): Seq[(Int, Int, Set[(SequenceID, Int)])]

  /**
    * Determines the occurrences of a pattern in this suffix tree
    *
    * @return A list of pairs containing the sequence ID and the position of
    *         the given sequence as a starting index (starting from 0, else
    *         an empty list
    *
    */
  def find(pattern: Repr): List[(SequenceID, Int)]

  /**
    * Determines if a pattern is contained in the suffix tree
    *
    */
  def contains(pattern: Repr): Boolean

  /**
    * Computes the multiple common subsequence from this generalized suffix
    * tree.
    *
    * This method is NOT efficient for large tree, see `bulkMultipleCommonSubsequence`
    * instead.
    *
    * Pre-processing of this algorithm has a linear-time complexity. It is
    * fully described in section 9.7 "A linear-time solution to the multiple
    * common substring problem" of Dan Gusfield's book "Algorithms on Strings,
    * Trees, and Sequences" (1997).
    *
    */
  def multipleCommonSubsequence(): CommonSubsequences[Repr]

  /**
    * Efficiently computes an iterator over the multiple common subsequences
    * from this generalized suffix tree.
    *
    * Pre-processing of this algorithm has a linear-time complexity. It is
    * fully described in section 9.7 "A linear-time solution to the multiple
    * common substring problem" of Dan Gusfield's book "Algorithms on Strings,
    * Trees, and Sequences" (1997).
    *
    * @return An iterator over the sequences along with their frequencies
    */
  def bulkMultipleCommonSubsequence(): Iterator[(Int, Repr)]

}

/**
  * Results of a common subsequence search
  *
  * @author Guillaume Dubuisson Duplessis
  *
  */
trait CommonSubsequences[Repr] {
  outer =>
  /**
    * A set of the observed number of occurrences comprised between 2 and K
    * (K being the number of sequences in the generalized suffix tree).
    *
    */
  def occurrences: Set[Int]

  /**
    *
    * @return An iterator over the sequences appearing exactly 'occ' times
    */
  def apply(occ: Int): Iterator[Repr]

  /**
    *
    * @return the number of subsequences appearing exactly 'occ' times
    */
  def nb(occ: Int): Int

  /**
    *
    * @return the size of the longest subsequences appearing exactly 'occ' times
    */
  def longest(occ: Int): Int

  def map[T](f: Repr => T): CommonSubsequences[T] =
    new CommonSubsequences[T] {
      def occurrences = outer.occurrences

      def nb(occ: Int) = outer.nb(occ)

      def longest(occ: Int): Int =
        outer.longest(occ)

      def apply(occ: Int): Iterator[T] = {
        val it: Iterator[Repr] = outer.apply(occ)
        it.map {
          x => f(x)
        }
      }
    }

  /**
    * Computes an iterator over the multiple common subsequences from this
    * generalized suffix tree.
    *
    * @return An iterator over the sequences along with their frequencies
    */
  def toIterator: Iterator[(Int, Repr)] =
    for {
      occ <- occurrences.toIterator
      subpattern <- this (occ)
    } yield ((occ, subpattern))

}

object CommonSubsequences {
  /**
    * Represents a CommonSubsequences result for an empty generalized suffix tree
    * @tparam Repr
    * @return
    */
  def empty[Repr]: CommonSubsequences[Repr] =
    new CommonSubsequences[Repr] {
      override def occurrences: Set[Int] = Set.empty

      override def longest(occ: Int): Int = 0

      override def apply(occ: Int): Iterator[Repr] = Iterator.empty

      override def nb(occ: Int): Int = 0
    }
}

object GeneralizedSuffixTree {

  /**
    * Creates an empty generalized suffix tree
    * @tparam Alphabet type of the items in the sequences
    * @tparam Repr type of the sequences
    * @return a new empty generalized suffix tree
    */
  def empty[Alphabet, Repr <% Sequence[Alphabet]]: GeneralizedSuffixTree[Alphabet, Repr] = new GeneralizedSuffixTree[Alphabet, Repr] {
    def getSequenceBy(id: SequenceID): Repr =
      throw new UnsupportedOperationException("Empty tree does not contain any sequence")

    val size = 0

    def fullSequences(): Iterator[Repr] = Iterator.empty

    def suffixes(): Iterator[Repr] = Iterator.empty

    val nSuffixes = 0

    def findLongestCommonSubsequences(pattern: Repr): Seq[(Int, Int, Set[(SequenceID, Int)])] =
      Seq.empty

    def find(pattern: Repr): List[(SequenceID, Int)] = List.empty

    def contains(pattern: Repr): Boolean = false

    def multipleCommonSubsequence(): CommonSubsequences[Repr] =
      CommonSubsequences.empty

    def bulkMultipleCommonSubsequence(): Iterator[(Int, Repr)] =
      Iterator.empty
  }

  /**
    * Creates a generalized suffix tree with the specified sequences
    *
    * @param sequences the sequences of the created generalized suffix tree
    * @tparam Alphabet type of the items in the sequences
    * @tparam Repr     type of the sequences
    * @return a new generalized suffix tree with the specified sequences
    */
  def apply[Alphabet, Repr <% Sequence[Alphabet]](sequences: Repr*)(implicit icbf: CanBuildFrom[Repr, Alphabet, Repr]): GeneralizedSuffixTree[Alphabet, Repr] = {
    if(sequences.nonEmpty) {
      val stree = GeneralizedSuffixTreeBuilder.empty[Alphabet, Repr]()

      for (item <- sequences) {
        stree.insert(item)
      }

      stree
    } else {
      GeneralizedSuffixTree.empty
    }
  }

  /**
    * Creates a new builder for a generalized suffix tree
    *
    * @tparam Alphabet type of the items in the sequences
    * @tparam Repr     type of the sequences
    * @return a new builder for a generalized suffix tree
    */
  def newBuilder[Alphabet, Repr <% Sequence[Alphabet]](
                                                        implicit icbf: CanBuildFrom[Repr, Alphabet, Repr]): mutable.Builder[Repr, GeneralizedSuffixTree[Alphabet, Repr]] =
    GeneralizedSuffixTreeBuilder.empty[Alphabet, Repr]()


  /**
    * Computes the longest subsequence of two given sequence if it exists
    *
    * @return an option consisting of the longest subsequence, or None if
    *         it does not exist
    */
  def longestSubsequence[Alphabet, Repr <% Sequence[Alphabet]](
                                                                seq1: Repr,
                                                                seq2: Repr)(
                                                                implicit icbf: CanBuildFrom[Repr, Alphabet, Repr]): Option[Repr] = {
    //
    val stree = GeneralizedSuffixTreeBuilder.empty[Alphabet, Repr]()
    stree.insert(seq1)
    stree.insert(seq2)
    val it = (stree.multipleCommonSubsequence()) (2)
    if (it.hasNext) {
      Some(it.next())
    } else {
      None
    }
  }
}
