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

import gstlib.GeneralizedSuffixTreeBuilder.Sequence

import scala.collection.generic.CanBuildFrom

/**
  * Represents a generic suffix tree built from a sequence of items
  *
  * @tparam Alphabet type of the item
  * @tparam Repr type of the sequence of items
  */
sealed trait SuffixTree[Alphabet, Repr] {
  /**
    * Computes an iterator over the suffixes of this suffix tree
    */
  def suffixes(): Iterator[Repr]

  /**
    * The sequence from which this suffix tree is built
    * @return the sequence from which this suffix tree is built
    */
  def sequence: Repr

  /**
    * Determines the occurrences of a pattern in this suffix tree
    *
    * @return A list of position of the given sequence as a starting index (starting from 0), else an empty list
    *
    */
  def find(pattern: Repr): List[Int]

  /**
    * Determines if a pattern is contained in the suffix tree
    * @return true if the pattern can be found in the suffix tree, else false
    */
  def contains(pattern: Repr): Boolean
}

object SuffixTree {
  /**
    * Creates an empty suffix tree
    * @tparam Alphabet type of an item
    * @tparam Repr type of the sequence of items
    * @return an empty suffix tree
    */
  def empty[Alphabet, Repr <% Sequence[Alphabet]] : SuffixTree[Alphabet, Repr] = {
    new SuffixTree[Alphabet, Repr] {
      def sequence = throw new UnsupportedOperationException("No available sequence in this empty suffix tree")

      def suffixes(): Iterator[Repr] =
        Iterator.empty

      def find(pattern: Repr): List[Int] =
        List.empty

      def contains(pattern: Repr): Boolean =
        false
    }
  }

  /**
    * Builds a suffix tree from a sequence of items
    * @param seq sequence of items from which the suffix tree is built
    * @tparam Alphabet type of an item
    * @tparam Repr type of the sequence of items
    * @return a suffix tree of the given sequence
    */
  def apply[Alphabet, Repr <% Sequence[Alphabet]](seq: Repr)(implicit icbf: CanBuildFrom[Repr, Alphabet, Repr]): SuffixTree[Alphabet, Repr] = {
    if(seq.nonEmpty) {

      val stree = GeneralizedSuffixTreeBuilder.empty[Alphabet, Repr]()
      stree.insert(seq)

      new SuffixTree[Alphabet, Repr] {
        val sequence = seq

        def suffixes(): Iterator[Repr] =
          stree.suffixes()

        def find(pattern: Repr): List[Int] =
          stree.find(pattern).map(_._2)

        def contains(pattern: Repr): Boolean =
          stree.contains(pattern)
      }
    } else {
      SuffixTree.empty
    }
  }
}
