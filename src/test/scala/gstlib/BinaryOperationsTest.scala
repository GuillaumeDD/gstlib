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
import BinaryOperations._

/**
 * @author Guillaume Dubuisson Duplessis
 */
class BinaryOperationsTest extends FunSuite {

  test("shiftRight") {
    assert(shiftRight(2, 1) == 1)
    assert(shiftRight(1, 1) == 0)
  }

  test("shiftLeft") {
    assert(shiftLeft(2, 1) == 4)
    assert(shiftLeft(1, 1) == 2)
  }

  test("height") {
    assert(height(8) == 4) // 1000
    assert(height(5) == 1) // 101
    assert(height(4) == 3) // 100
    assert(height(1) == 1) // 1
    assert(height(10) == 2) // 1010
  }

  test("leftMost1Bit") {
    assert(leftMost1Bit(1, 2) == 2) // 01 -> 2
    assert(leftMost1Bit(1, 20) == 20) // 00...01 -> 20
    assert(leftMost1Bit(2, 2) == 1) // 10 -> 1 
    assert(leftMost1Bit(2, 3) == 2) // 010 -> 2
    assert(leftMost1Bit(2, 4) == 3) // 0010 -> 3 
    assert(leftMost1Bit(5, 4) == 2) // 0101 -> 2
    assert(leftMost1Bit(5, 3) == 1) // 101 -> 1
    assert(leftMost1Bit(6, 3) == 1) // 110 -> 1
    assert(leftMost1Bit(6, 4) == 2) // 0110 -> 2
  }

  test("rightMost1Bit") {
    assert(rightMost1Bit(1) == 1) // 01 -> 1
    assert(rightMost1Bit(2) == 2) // 10 -> 2
    assert(rightMost1Bit(3) == 1) // 11 -> 1
    assert(rightMost1Bit(4) == 3) // 100 -> 3
    assert(rightMost1Bit(8) == 4) // 1000 -> 4
    assert(rightMost1Bit(9) == 1) // 1001 -> 1
    assert(rightMost1Bit(10) == 2) // 1010 -> 2  
  }

  test("leftMost1BitBelow") {
    assert(leftMost1BitBelow(10, 4) == 2) //1010 -> 2
    assert(leftMost1BitBelow(9, 4) == 1) // 1001 -> 1
    assert(leftMost1BitBelow(15, 3) == 2) //1111 -> 2
    assert(leftMost1BitBelow(9, 3) == 1) // 1001 -> 1
    assert(leftMost1BitBelow(1, 3) == 1) // 0001 -> 1
  }

  test("clearBefore") {
    assert(clearBefore(10, 2) == 8) // 1010 -> 1000 = 8
    assert(clearBefore(15, 2) == 12) // 1111 -> 1100 = 12
    assert(clearBefore(1, 2) == 0) // 0001 -> 0
  }

  test("toggleBitTo1") {
    assert(toggleBitTo1(10, 2) == 10) // 1010 -> 1010 = 10
    assert(toggleBitTo1(10, 3) == 14) // 1010 -> 1110 = 14
    assert(toggleBitTo1(15, 4) == 15) // 1111 -> 1111 = 15
    assert(toggleBitTo1(15, 5) == 31) // 1111 -> 11111 = 31
    assert(toggleBitTo1(0, 5) == 16) // 0 -> 10000 = 16  
  }

  test("A") {
    assert(A(List[Int]()) == 0) // 0
    assert(A(List(1, 4)) == 9) // 9
    assert(A(List(2, 3)) == 6) // 6
    assert(A(List(2)) == 2) // 2
    assert(A(List(2, 2, 2, 2)) == 2) // 2
  }

  test("bitCompareA") {
    assert(bitCompareA(1, 2) == List()) // 01 comp 10 => List()
    assert(bitCompareA(10, 9) == List(4)) // 1010 comp 1001 => List(4)
    assert(bitCompareA(10, 11) == List(4, 2)) // 1010 comp 1001 => List(4, 2)  
  }

  test("CompleteBinaryTree.numberOfChildren") {
    assert(CompleteBinaryTree.numberOfChildren(4) == 7)
  }

  test("CompleteBinaryTree.ancestor") {
    assert(CompleteBinaryTree.ancestor(9, 6, 4) == None) // None
    assert(CompleteBinaryTree.ancestor(6, 9, 4) == None) // None
    assert(CompleteBinaryTree.ancestor(9, 10, 4) == Some(10)) // Some(10)
    assert(CompleteBinaryTree.ancestor(10, 9, 4) == Some(10)) // Some(10)
    assert(CompleteBinaryTree.ancestor(7, 8, 4) == Some(8)) // Some(8)
    assert(CompleteBinaryTree.ancestor(8, 7, 4) == Some(8)) // Some(8)
    assert(CompleteBinaryTree.ancestor(7, 7, 4) == Some(7)) // Some(7)
  }

  test("CompleteBinaryTree.lca") {
    /*
   *                   100(4)
   *
   *          010(2)           110(6)
   *
   *     001(1)    011(3)  101(5)    111(7)
   *
   *
   */

    // 4 is the root   

    assert(CompleteBinaryTree.lca(4, 2, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(2, 4, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(4, 1, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(1, 4, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(4, 3, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(3, 4, 3) == 4) // 4

    assert(CompleteBinaryTree.lca(4, 5, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(5, 4, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(4, 6, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(6, 4, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(4, 7, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(7, 4, 3) == 4) // 4

    assert(CompleteBinaryTree.lca(1, 3, 3) == 2) // 2
    assert(CompleteBinaryTree.lca(3, 1, 3) == 2) // 2
    assert(CompleteBinaryTree.lca(5, 7, 3) == 6) // 6
    assert(CompleteBinaryTree.lca(7, 5, 3) == 6) // 6

    assert(CompleteBinaryTree.lca(3, 5, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(5, 3, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(1, 7, 3) == 4) // 4
    assert(CompleteBinaryTree.lca(7, 1, 3) == 4) // 4
  }
}