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

import scala.annotation.tailrec

/**
  * Implements operations at the binary representation level and on complete binary trees
  *
  * @author Guillaume Dubuisson Duplessis
  */
object BinaryOperations {
  /**
    * Shift-right by a given number of digit
    * E.g.,
    *
    * 1001 --shift-right-3--> 0001
    *
    */
  def shiftRight(n: Int, nbBit: Int): Int =
    n >>> nbBit

  /**
    * Shift-left by a given number of digit
    * E.g.,
    *
    * 1001 --shift-left-1--> 10010
    *
    */
  def shiftLeft(n: Int, nbBit: Int): Int =
    n << nbBit

  def setRightMostBitTo1(n: Int): Int =
    if (n % 2 == 0) {
      n + 1
    } else {
      n
    }

  /**
    * Computes the position (counting from the right) of the least-significant
    * 1-bit in the binary representation of the given k
    *
    * In other words, height(k) is given by: 1 + the number of consecutive
    * zeros at the right end of k.
    *
    * E.g.:
    *  - h(8) = 4 (8 is 1000)
    *  - h(5) = 1 (5 is 0101)
    *  - h(4) = 3 (4 is 0100)
    *  - h(1) = 1 (1 is 0001)
    *
    * @return the position (counting from the right) of the least-significant
    *         1-bit in the binary representation of the given k
    */
  def height(k: Int): Int = {
    require(k > 0, s"Impossible to compute height of k=$k")

    var result = 0
    var quotient = k
    var rest = 0
    do {
      result += 1
      rest = quotient % 2
      quotient = quotient / 2
    } while (rest == 0)

    result
  }

  /**
    * Create a bit number A with the given heights.
    * Bit A(i-1) is set to 1 if and only if i is in the given collection.
    *
    * E.g.:
    *  - List(1,4) generates: 1001 = 9
    *  - List(2,3) generates: 0110 = 6
    *  - List(2) generates: 0010 = 2
    *  - List(2,2,2,2) generates: 0010 = 2
    *
    * @param heights a collection of integer > 0
    * @return bit number A
    */
  def A(heights: Iterable[Int]): Int = {
    val correctedHeights = heights.toSet
    correctedHeights.map(i =>
      1 << (i - 1) // 2^(i-1)
    ).sum
  }

  /**
    * Computes the positions where both a1 and a2 have 1-bit (starting from 1, counting from right to left)
    * in descending order.
    *
    * E.g.,
    *  - 01 compared to 10 gives List()
    *  - 1010 compared to 1001 gives List(4)
    *  - 1010 compared to 1011 gives List(4, 2)
    *
    */
  def bitCompareA(a1: Int, a2: Int): List[Int] = {
    require(a1 > 0)
    require(a2 > 0)

    @tailrec
    def compareHelper(a1: Int, a2: Int, pos: Int, acc: List[Int]): List[Int] = {
      if (a1 > 0 && a2 > 0) {
        if (a1 % 2 == 1 && a2 % 2 == 1) {
          compareHelper(shiftRight(a1, 1), shiftRight(a2, 1), pos + 1, pos :: acc)
        } else {
          compareHelper(shiftRight(a1, 1), shiftRight(a2, 1), pos + 1, acc)
        }
      } else {
        acc
      }
    }

    compareHelper(a1, a2, 1, List())
  }

  /**
    * Computes the position of the left-most 1 bit of a given strictly positive
    * integer (counting from LEFT), on a binary representation of a certain size.
    *
    * Bits are identified by a number starting from 1 and going to nbBit.
    * E.g., here is an example with nbBit=5:
    * 00111
    * 12345
    *
    * Results is 3.
    *
    * @param n input number
    * @param nbBit size of the binary representation
    * @return position of the left-most 1 bit counting from left and starting from 1
    */
  def leftMost1Bit(n: Int, nbBit: Int): Int = {
    require(nbBit > 0, s"Binary representation should be > 0 (nbBit=$nbBit)")
    require(n > 0, s"Invalid integer, it should be > 0 (integer=$n)")

    /*
     * Idea of the algorithm: 
     * Find the biggest power of two between 2^(nbBit - 1) and 2^0 (decreasing) 
     * that fits in the given n. 
     * 
     */
    var position = 0
    var found = false
    var i = nbBit - 1

    while (!found && i >= 0) {
      position += 1
      val pow2 = 1 << i
      if (n - pow2 >= 0) {
        found = true
      }

      i -= 1
    }

    position
  }

  /**
    * Computes the position of the right-most 1-bit of a given strictly positive
    * integer (counting from right)
    *
    * Bits are identified by a number starting from 1.
    * E.g.:
    * 110
    * 321
    *
    * Results is 2.
    *
    * @param n input number
    * @return position of the left-most 1 bit counting from left and starting from 1
    */
  def rightMost1Bit(n: Int): Int = {
    require(n > 0, s"Invalid integer, it should be > 0 (integer=$n)")

    var nTemp = n
    var position = 0
    var found = false

    while (nTemp > 0 && !found) {
      position += 1
      /*
       * nTemp & 00.00001 == 1 iff nTemp = ....1
       */
      if ((nTemp & 1) == 1) {
        found = true
      }
      nTemp = shiftRight(nTemp, 1) // continue with the next bit
    }

    position
  }

  /**
    * Computes the position of the left-most 1-bit of a given pos (> 1) on a strictly
    * positive integer (counting from right).
    *
    * E.g.:
    * pos: 7654321
    * n  : 1010011
    *
    * With position 5, result is 2.
    *
    */
  def leftMost1BitBelow(n: Int, pos: Int): Int = {
    require(n > 0, s"n should be > 0 (n=$n)")
    require(pos > 1, s"Bit position should be > 1 (pos=$pos)")
    /*
     * Creation of a mask to keep every bit below pos (excluded)
     *   7654321
     *   1010011
     *     ^
     *    pos
     * & 0001111 = 2^(j-1) - 1 (mask)
     * ---------
     *   0000011
     */
    val mask = (1 << (pos - 1)) - 1
    var nTemp = mask & n

    // Find greatest position for 1-bit
    var position = 0
    var rightMostBelowPos = 0
    while (nTemp > 0) {
      position += 1
      /*
       * nTemp & 00.00001 == 1 iff nTemp = ....1
       */
      if ((nTemp & 1) == 1) {
        rightMostBelowPos = position
      }
      nTemp = shiftRight(nTemp, 1) // continue with the next bit
    }

    rightMostBelowPos
  }

  /**
    * Toggle to 0-bit the 'pos' first bit of a given integer (counting from right)
    *
    * E.g.:
    * pos: 7654321
    * n  : 1010011
    *
    * With position 4, result is 1010000.
    *
    * @return
    */
  def clearBefore(n: Int, pos: Int): Int = {
    require(pos > 0, s"Bit position should be > 1 (pos=$pos)")
    /*
     * Creation of a mask to keep every bit below pos (excluded)
     *   7654321
     *   1010011
     *     ^
     *    pos
     * & 1100000 = ~(2^j - 1) (mask)
     * ---------
     *   1000000
     */
    val mask = ~((1 << pos) - 1)
    n & mask
  }

  /**
    * Toggle to bit 1 the 'pos' bit of a given integer (counting from right)
    *
    * E.g.:
    *  - binary representation of n is 1000 and pos=1 returns 1001
    *  - binary representation of n is 1000 and pos=4 returns 1000
    *  - binary representation of n is 1000 and pos=5 returns 11000
    *
    */
  def toggleBitTo1(n: Int, pos: Int): Int = {
    val mask = (-1 ^ n) & (1 << (pos - 1))
    n ^ mask
  }

  /**
    * Implements operations on complete binary trees
    */
  object CompleteBinaryTree {

    /**
      * Number of children of a node v (including itsef) in a complete
      * binary tree identified by its inorder traversal number (starting
      * from 1).
      *
      * @param node node identified by its inorder traversal number
      *
      */
    def numberOfChildren(node: Int): Int = {
      val h = height(node)

      if (h == 1) {
        1
      } else {
        /*
       * Geometric progression:
       * SUM for i=0 to h=height(v) of
       * 1  + 2  + 4  + ... + 2^h
       * v0   v1 + v2 + ... + v(h-1)
       * = v0 * (1 - 2^h) / (1 - 2)
       * = -(1 - 2^h) = 2^h - 1
       *
       */

        (1 << h) - 1
      }
    }

    /**
      * Determines if one of the two nodes i or j is an ancestor of the other, and
      * in this case return the ancestor.
      *
      * Nodes are identified by their inorder traversal ordering number
      * (starting from 1).
      *
      * @param i number of node i
      * @param j number of node j
      * @param d size of the binary representation of i and j (which is also the height
      *          of the complete binary tree starting from height 1 at leaves)
      * @return Some ancestor between i and j if it exists, else None
      *
      */
    def ancestor(i: Int, j: Int, d: Int): Option[Int] = {
      /*
       * Using i and j as "path-number" :
       * 
       * In the special case of a complete binary tree, there is an alternative
       * way to handle the situation when lca(i,j) is i or j. Using h(i) and h(j)
       * we can determine which of the nodes i and j is higher in the tree (say i)
       * and how many edges are on the path from the root to node i. Then we take
       * the XOR of the binary for i and for j and find the left-most 1-bit, say in
       * position k (counting from the left). Node i is an ancestor of j if and only
       * if k is larger than the number of edges on the path to node i.
       * 
       */
      if (i == j) {
        Some(i)
      } else {
        val hi = height(i)
        val hj = height(j)

        // Size of the binary representation
        val iXORj = i ^ j
        val k = leftMost1Bit(iXORj, d)
        if (hi > hj) {
          val numberOfEdgeFromRootToI = d - hi
          if (k > numberOfEdgeFromRootToI) {
            Some(i)
          } else {
            None
          }
        } else {
          val numberOfEdgeFromRootToJ = d - hj
          if (k > numberOfEdgeFromRootToJ) {
            Some(j)
          } else {
            None
          }
        }
      }
    }

    /**
      * Computes the lowest common ancestor (lca) of two given nodes
      * in a complete binary tree.
      *
      * Nodes are identified by their inorder traversal ordering number
      * (starting from 1).
      *
      * @param i number of node i
      * @param j number of node j
      * @param d size of the binary representation of i and j (which is also the height
      *          of the complete binary tree starting from height 1 at leaves)
      * @return the lca of i and j
      *
      */
    def lca(i: Int, j: Int, d: Int): Int = {
      ancestor(i, j, d) match {
        case Some(node) => // Case: i is ancestor of j, or vice-versa
          node

        case None => // Case: lca(i,j) is neither i nor j
          val xor = i ^ j
          // println(s"xor: ${xor.toBinaryString}")
          val k = leftMost1Bit(xor, d)
          // println(s"k: ${k}")
          val shifting = d - k
          // println(s"shifting: ${shifting}")
          // println(s"shift-right: ${shiftRight(i, shifting).toBinaryString}")
          // println(s"right-most 1-bit: ${setRightMostBitTo1(shiftRight(i, shifting)).toBinaryString}")
          // println(s"shift-left: ${shiftLeft(setRightMostBitTo1(shiftRight(i, shifting)), shifting).toBinaryString}")
          /*
           *  Shift i right by d - k places
           *  Set the right most bit to a 1
           *  Shift it back left by d - k places 
           */
          shiftLeft(setRightMostBitTo1(shiftRight(i, shifting)), shifting)
      }
    }
  }

}
