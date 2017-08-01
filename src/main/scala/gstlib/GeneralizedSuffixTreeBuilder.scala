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
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import GeneralizedSuffixTreeBuilder._
import InnerTree._

/**
  * Represents a generalized suffix tree builder in which it is possible to insert
  * several sequences of items.
  *
  * @tparam Alphabet type of the items in the sequences
  * @tparam Repr type of the sequences
  */
protected[gstlib] sealed abstract class GeneralizedSuffixTreeBuilder[Alphabet, Repr <% Sequence[Alphabet]]
  extends GeneralizedSuffixTree[Alphabet, Repr]
    with mutable.Builder[Repr, GeneralizedSuffixTree[Alphabet, Repr]] {
  implicit val cbf: CanBuildFrom[Repr, Alphabet, Repr]

  def +=(elem: Repr): GeneralizedSuffixTreeBuilder.this.type ={
    this.insert(elem)
    this
  }

  def result(): GeneralizedSuffixTree[Alphabet, Repr] = {
    this
  }

  def clear(): Unit = {
    this.nodeCount = 0
    this._sequences.clear()
    this.leaves.clear()

    this._seqID = -1

    this.root = newRoot()

    toggleRequirePreprocessing()
  }


  protected var nodeCount = 0

  protected def countNewNode(): Unit = {
    nodeCount += 1
  }

  protected def numberOfNodes(): Int = nodeCount

  private def newRoot(): Root = {
    countNewNode()
    Root()
  }

  protected var root: Root = newRoot()

  def size(): Int =
    _sequences.size

  def find(pattern: Repr): List[(SequenceID, Int)] =
    root.walkDownUntilUnmatch(WrappedSequence(pattern), 0, pattern.length - 1)(start, getItemLabel, labelLength) match {
      case None =>
        List()
      case Some((_, nbCharacterLeft, _)) if nbCharacterLeft > 0 =>
        List()
      case Some((node, _, _)) =>
        node.leaves().map {
          case LeafNode(_, seqID, _, n, _) =>
            (seqID, n - 1)
        }
    }

  def findLongestCommonSubsequences(pattern: Repr): Seq[(Int, Int, Set[(SequenceID, Int)])] = {
    val endIndex = pattern.length - 1

    // Helper function to search in the generalized suffix tree from a starting position in the pattern
    def findLongestCommonSubsequenceHelper(startIndex: Int): Option[(Int, Int, Set[(SequenceID, Int)])] = {
      // researching in the tree
      root.walkDownUntilUnmatch(WrappedSequence(pattern), startIndex, endIndex)(start, getItemLabel, labelLength) match {
        case None => // cannot find the pattern
          None

        case Some((nonRootNode, nbElementLeft, nbElementToReachNode)) => // a part of the pattern has been found
          val leaves = nonRootNode.leaves().map({
            case LeafNode(_, seqID, _, n, _) =>
              (seqID, n - 1) // Minus 1 to start from index 0
          })
          Some((startIndex,
            endIndex - nbElementLeft + 1,
            leaves.toSet))
      }
    }

    /*
     * For every suffix of the pattern, search the presence of its biggest prefix in the tree.
     * Then, keep the biggest prefix of the suffixes
     */
    var maxSize = 0
    val resultsForSuffixes = for {
      startI <- 0 to endIndex
      result <- findLongestCommonSubsequenceHelper(startI)
      // cutting the search if the subsequence is smaller than the previously found subsequences
      if maxSize < (pattern.length - startI + 1)
    } yield {
      if (maxSize < (result._2 - result._1)) {
        maxSize = result._2 - result._1
      }
      result
    }

    // Keeping only the suffixes of max size
    resultsForSuffixes.filter({
      case (start, end, _) =>
        (end - start) == maxSize
    })

  }

  def contains(pattern: Repr): Boolean =
    root.walkDownUntilUnmatch(WrappedSequence(pattern), 0, pattern.length - 1)(start, getItemLabel, labelLength) match {
      case None =>
        false
      case Some((_, nbCharacterLeft, _)) if nbCharacterLeft > 0 =>
        false
      case Some((node, _, _)) =>
        true
    }

  def fullSequences(): Iterator[Repr] =
    _sequences.iterator.map { wrappedSeq => wrappedSeq.seq }

  def suffixes(): Iterator[Repr] = {
    val l = leaves.map { leaf =>
      val path = leaf.pathSingleLabel(s => getSequenceLengthBy(s) - 1 // removing the last item of the sequence (suffix stopper)
      ) // Building of the path label (bottom-up)
      CompositeLabel(path) // Turn the path-label to a definite label
    }.filter(_.length > 0)

    new Iterator[Repr] {
      private var suffixes = l

      def hasNext: Boolean =
        suffixes.nonEmpty

      def next(): Repr = {
        val item = suffixes.head
        suffixes = suffixes.tail
        item.value[Alphabet, Repr](getSubsequenceBy)
      }
    }
  }

  /*
   * GENERALIZED SUFFIX TREE BUILDER METHODS
   *  
   */
  /**
    * Computes an edge-label on a built generalized suffix tree
    *
    */
  protected def edgeLabel(node: NonRootNode): Repr =
    edgeLabelPartial(_seqID + 1, 0)(node)

  /**
    * Computes the length of an edge-label on a built generalized suffix tree
    *
    */
  protected def labelLength(node: NonRootNode): Int =
    labelLengthPartial(_seqID + 1, 0)(node)

  /**
    * Computes the full label from the root to the given node on a built generalized
    * suffix tree
    *
    */
  protected def label(node: NonRootNode): Repr =
    labelPartial(_seqID + 1, 0)(node)

  /**
    * Computes an informative label containing the edge-label, the path from the root
    * and the count C(node) if it exists on a built generalized suffix tree
    *
    */
  protected def labels(node: NonRootNode): String = {
    val cv = if (_prepared_for_MCSA) {
      val nodeDFS = nodeToDFS(node)
      s"C(v)=${C(nodeDFS)}"
    } else {
      "???"
    }
    s"edge: ${edgeLabel(node)}; path: ${label(node)}; $cv"
  }

  /**
    * Computes the lowest common ancestor of two given nodes
    * (constant-time LCA retrieval)
    *
    */
  protected def lca(node1: Node, node2: Node): Node = {
    val x = nodeToDFS(node1)
    val y = nodeToDFS(node2)
    dfsToNode(lca(x, y))
  }

  /**
    * Computes the lowest common ancestor to two given nodes identified by
    * their depth-first number (constant-time LCA retrieval)
    *
    */
  protected def lca(x: Int, y: Int): Int = {
    preProcessingLCA() // do the preprocessing if not already done

    val Ix = I(x)
    val Iy = I(y)

    // 1. Find the lowest common ancestor b in Beta of nodes I(x) and I(y)
    val b = BinaryOperations.CompleteBinaryTree.lca(Ix, Iy, d)

    /*
     *  2. Find the smallest position j greater than or equal to h(b) such
     *  that both numbers Ax and Ay have 1-bit in position j.
     */
    val hb = h(b)
    val Ax = A(x)
    val Ay = A(y)
    val j = // Computation of j
      BinaryOperations.bitCompareA(Ax, Ay) // List of position in descending order
        .reverse
        .dropWhile { pos => pos < hb } // drop the position strictly inferior to h(b)
        .head // take the first position >= h(b)

    /*
     * 3. Find node xBar, the closest node to x on the same run as z (although
     * we don't know z)
     */
    val lx = BinaryOperations.rightMost1Bit(Ax)
    val xBar =
      if (lx == j) {
        x
      } else {
        // Find the position k of the left-most 1-bit in Ax that is to the right of position j
        val k = BinaryOperations.leftMost1BitBelow(Ax, j)
        /*
         *  Form the number consisting of the bits of I(x) to the left of position k,
         *  followed by a 1-bit in position k, followed by all zeros
         */
        val iw = BinaryOperations.toggleBitTo1(BinaryOperations.clearBefore(Ix, k), k)
        // Look up node L(I(w)) which must be node w
        val w = L(iw)
        // Set node xBar to be the parent of node w in T
        nodeToDFS(dfsToNode(w).asInstanceOf[NonRootNode].parent)
      }

    // 4. Find node yBar the closest node to y on the same run as z (same approach as 3.)
    val ly = BinaryOperations.rightMost1Bit(Ay)
    val yBar =
      if (ly == j) {
        y
      } else {
        // Find the position k of the left-most 1-bit in Ay that is to the right of position j
        val k = BinaryOperations.leftMost1BitBelow(Ay, j)
        /*
         *  Form the number consisting of the bits of I(y) to the left of position k,
         *  followed by a 1-bit in position k, followed by all zeros
         */
        val iw = BinaryOperations.toggleBitTo1(BinaryOperations.clearBefore(Iy, k), k)
        // Look up node L(I(w)) which must be node w
        val w = L(iw)
        // Set node yBar to be the parent of node w in T
        nodeToDFS(dfsToNode(w).asInstanceOf[NonRootNode].parent)
      }

    // 5. if xBar < yBar then set z to xBar, else set z to yBar
    if (xBar < yBar) {
      xBar
    } else {
      yBar
    }
  }

  private var _prepared_for_lca = false

  /**
    * Mapping: node to depth-first search numbers
    *
    */
  object nodeToDFS {
    def apply(node: Node): Int =
      node.dfs

    def update(node: Node, value: Int): Unit =
      node.dfs = value

    def init(): Unit = {}
  }

  /**
    * Number of nodes by their depth-first number in the tree
    */
  protected def N: Int =
    if (_prepared_for_lca) {
      this.numberOfNodes()
    } else {
      throw new Error("Accessing N while LCA pre-processing has not been done!")
    }

  /**
    * Computes the set of nodes by their depth-first number (including root, internal nodes and
    * leaf nodes)
    */
  protected def dfsNodes(): Iterable[Int] =
    1 to _dfsToNode.length // note the translation of indices (+1)

  /**
    * Computes the set of internal nodes by their depth-first number (i.e., internal nodes and leaf
    * nodes but not the root)
    */
  protected def dfsInternalNodes(): Iterable[Int] = {
    val rootDFS = nodeToDFS(root)
    dfsNodes().filter(node => node != rootDFS)
  }

  /**
    * Mapping: node to depth-first search numbers
    *
    */
  private var _dfsToNode: Array[Node] = Array.empty[Node]

  protected def dfsToNode(dfs: Int): Node =
    _dfsToNode(dfs - 1)

  // note: translation -1

  object dfsToNode {
    def update(index: Int, value: Node): Unit =
      _dfsToNode(index - 1) = value

    def init(): Unit =
      _dfsToNode = Array.ofDim(numberOfNodes())
  }

  /**
    * Mapping: node v dfs number -> h(v)
    */
  private def h(node: Int): Int =
    BinaryOperations.height(node)

  /**
    * Mapping: node v dfs number -> I(v)
    */
  private var _I = Array.empty[Int]

  object I {
    def apply(dfs: Int): Int =
      _I(dfs - 1)

    def update(dfs: Int, value: Int): Unit =
      _I(dfs - 1) = value

    def init(): Unit =
      _I = Array.fill(numberOfNodes())(-1) // initialisation at -1 means "do not contains"

    def contains(dfs: Int): Boolean =
      _I(dfs - 1) != -1
  }

  /**
    * Leaders: L(I(v)) is the head of the run containing an arbitrary node
    * v.
    */
  private var _L = Array.empty[Int]

  object L {
    def apply(dfs: Int): Int =
      _L(dfs - 1)

    def update(dfs: Int, value: Int): Unit =
      _L(dfs - 1) = value

    def init(): Unit =
      _L = Array.ofDim(numberOfNodes())
  }

  /**
    * Mapping: node v dfs number -> a bit number A
    */
  private var _A = Array.empty[Int]

  object A {
    def apply(dfs: Int): Int =
      _A(dfs - 1)

    def update(dfs: Int, value: Int): Unit =
      _A(dfs - 1) = value

    def init(): Unit =
      _A = Array.ofDim(numberOfNodes())
  }

  /**
    * Number of bit used for the representation of nodes in complete binary tree
    * Beta
    */
  private var d = 0

  /**
    * The string-depth of a node v is the number of items in v's label
    */
  private var _stringDepth = Array.empty[Int]

  object stringDepth {
    def apply(dfs: Int): Int =
      _stringDepth(dfs - 1)

    def update(dfs: Int, value: Int): Unit =
      _stringDepth(dfs - 1) = value

    def init(): Unit =
      _stringDepth = Array.ofDim(numberOfNodes())
  }

  /**
    * Clear variable pre-computed for the constant-time LCA retrieval process
    */
  protected def clearLCAPreProcessing(): Unit = {
    nodeToDFS.init()
    stringDepth.init()
    this.I.init()
    this.L.init()
    this.A.init()
    d = 0
  }

  /**
    * Part of the LCA pre-processing
    */
  protected def depthFirstNumberingAndHv(): Unit = {
    this.dfsToNode.init()

    var currentI = 1

    def numbering(node: Node): Unit = {
      // Computation of depth-first number
      nodeToDFS(node) = currentI
      dfsToNode(currentI) = node

      // Computation of string-depth
      if (node != root) {
        val currentNode = node.asInstanceOf[NonRootNode]
        val parentNode = nodeToDFS(currentNode.parent)
        val labelLength = this.labelLength(currentNode)

        stringDepth(currentI) = stringDepth(parentNode) + labelLength
      } else {
        // Initialisation of string-depth of the node
        stringDepth(currentI) = 0
      }

      // Continuation of the depth-first traversal
      currentI += 1
      for (child <- node.children) {
        numbering(child)
      }
    }

    // Depth-first numbering
    numbering(root)

    // Ordering leaves
    this.leaves = this.leaves.sortBy { leaf => nodeToDFS(leaf) }
  }

  /**
    * Part of the LCA pre-processing
    */
  protected def computeIvAndLeaders(): Unit = {
    def alreadyComputed(nodeDFS: Int): Boolean =
      I.contains(nodeDFS)

    @tailrec
    def bottomUpTraversal(candidates: mutable.Set[Int]): Unit = {
      if (candidates.nonEmpty) {
        val newCandidates = mutable.Set[Int]()
        for {
          v <- candidates
          realNode = dfsToNode(v)
        } {
          // Check that for each child v of the candidate node has a value of i(v)
          val childrenComputed = realNode.children.forall { child => I.contains(nodeToDFS(child)) }
          if (childrenComputed) {
            val hv = h(v)
            val children = realNode.children.map(nodeToDFS(_)) // mapping children to their DFS number

            /*
             * Computation of I(v)
             * 
             * For a node v of T, let I(v) be a node w in T such that h(w) is maximum over all nodes 
             * in the subtree of v (including v itself).
             * 
             */
            if (children.forall(child => hv >= h(I(child)))) {
              /*
               * For every internal node, I(v) = v if h(v) is greater than
               * h(v') for every child v' of v
               * 
               * Interpretation: h(v) >= h(I(w)) for every direct child w
               * 
               */
              I(v) = v
            } else {
              /*
               * Otherwise, I(v) is set to the I(v') value of the child v' whose h(I(v')) value is 
               * the maximum over all children of v.
               */
              val maxChild = children.maxBy { child => h(I(child)) }
              I(v) = I(maxChild)
            }

            // Computing leaders
            /*
             * Node u is identified as the head of its run if the I value of
             * u's parent is not I(u)
             * See root case below.
             */
            for {
              child <- children
              if I(child) != I(v)
            } {
              L(I(child)) = child
            }

            // Important operation to avoid recomputing a node
            // Remove from the next candidates the computed node            
            newCandidates(v) = false

            // new candidate: parent node of v
            if (realNode != root) {
              val parentNode = realNode.asInstanceOf[NonRootNode].parent
              val parentNodeDFS = nodeToDFS(parentNode)
              if (!alreadyComputed(parentNodeDFS)) {
                // avoid adding an already computed candidate
                newCandidates(parentNodeDFS) = true
              } else {
                newCandidates(parentNodeDFS) = false
              }
            } else {
              // Root is leader of its run
              L(I(v)) = v
            }
          } else {
            // v stays a candidate node
            newCandidates(v) = true
          }
        }

        bottomUpTraversal(newCandidates)
      }
    }

    /*
     * For every leaf v, I(v) = v
     */
    val parents = mutable.Set[Int]()
    for {
      leafNode <- leaves
      leaf = nodeToDFS(leafNode)
    } {
      I(leaf) = leaf
      val parent = nodeToDFS(leafNode.parent)
      parents(parent) = true
    }

    bottomUpTraversal(parents)
  }

  /**
    * Part of the LCA pre-processing
    */
  protected def computeA(): Unit = {
    def computeAHelper(node: Node, ancestor: List[Int]): Unit = {
      val dfs = nodeToDFS(node)
      // Adding the node itself to ancestors (it includes the node itself)
      val item = h(I(dfs))
      val newAncestor = if (ancestor.contains(item)) {
        ancestor
      } else {
        item :: ancestor
      }
      val a = BinaryOperations.A(newAncestor)
      A(dfs) = a

      for {
        child <- node.children
      } {
        computeAHelper(child, newAncestor)
      }
    }

    computeAHelper(root, List())
  }

  /**
    * Linear time pre-processing for LCA constant-time retrieval.
    *
    * This pre-processing is fully described in chapter 8 "Constant-Time Lowest
    * Common Ancestor Retrieval" of Dan Gusfield's book "Algorithms on Strings,
    * Trees, and Sequences" (1997).
    *
    */
  protected def preProcessingLCA(): Unit = {
    if (!_prepared_for_lca) {
      clearLCAPreProcessing()

      /*
       *  Do a depth-first traversal of the tree to assign dept-first search numbers
       *  to the nodes. During the traversal compute h(v) for each node v.
       */
      depthFirstNumberingAndHv()

      /*
       * Compute I(v) for each v.
       * For each number k such that I(v) = k for some node v, set L(k) to point to
       * the head (or Leader) of the run containing node k.
       * 
       */
      computeIvAndLeaders()

      /*
       * For each node v in T, create a bit number Av. Bit Av(i) is set to 1 if and 
       * only if node v has some ancestor in T that maps to height i in Beta, i.e.,
       * iff v has an ancestor u such that h(I(u)) = i
       */
      computeA()

      _prepared_for_lca = true

      d = (math.log(this.N) / math.log(2)).ceil.toInt
    }
  }

  protected def toggleRequirePreprocessing(): Unit = {
    _prepared_for_lca = false
    _prepared_for_MCSA = false
  }

  /*
   * MULTIPLE COMMON SUBSEQUENCE ALGORITHM METHODS
   * 
   */
  private var _prepared_for_MCSA = false

  /**
    * The number of distinct leaf string identifiers in the subtree of v
    *
    */
  var _C = Array.empty[Int]

  object C {
    def apply(dfs: Int): Int =
      _C(dfs - 1)

    def update(dfs: Int, value: Int): Unit =
      _C(dfs - 1) = value

    def init(): Unit =
      _C = Array.ofDim(numberOfNodes())
  }

  /**
    * The number of distinct leaf string identifiers in the subtree of v
    *
    */
  protected def C(n: Node): Int = {
    val v = nodeToDFS(n)
    C(v)
  }

  /**
    * Part of the MCSA pre-processing.
    *
    * Clear variable used in this processing ("clean slate")
    */
  protected def clearMCSAPreProcessing(): Unit = {
    C.init()
  }

  /**
    * Pre-processing of the Multiple Common Subsequence Algorithm (MCSA)
    *
    * This pre-processing is fully described in section 9.7 "A linear-time solution
    * to the multiple common substring problem" of Dan Gusfield's book "Algorithms
    * on Strings, Trees, and Sequences" (1997).
    *
    */
  protected def preprocessingMultipleCommonSubsequence(): Unit = {
    if (!_prepared_for_MCSA) {
      clearMCSAPreProcessing()

      /*
     *  Ensure that the pre-processing for LCA has been done
     *  This does the depth-first ordering.
     *   
     */
      preProcessingLCA()

      /**
        * Mapping: node dfs number -> LCA count
        *
        * A count of the number of times that the node identified by its depth-first
        * number (key of the map) is the computed LCA.
        *
        */
      val hLCAcount = Array.ofDim[Int](this.numberOfNodes() + 1) // +1 is due to the fact that we do not use cell indexed by 0
      /**
        * Mapping: sequence ID -> ordered list of leaves with identifier i
        *
        */
      val listOfLeaves = Array.ofDim[mutable.ListBuffer[Int]](this.size())

      /*
     *  3. For each string identifier i, extract the ordered list L of leaves
     *     with identifier i
     */
      for (seqID <- this.getSequenceIDS()) {
        listOfLeaves(seqID) = mutable.ListBuffer()
      }

      for {
        leaf <- this.leaves
        seqID = leaf.sequenceID
        dfsNumber = nodeToDFS(leaf)
      } {
        listOfLeaves(seqID).append(dfsNumber) // leaves are already sorted
      }

      debugging {
        for (listOfLeaves <- listOfLeaves) {
          assert(listOfLeaves == listOfLeaves.sorted, s"A list of leaves is not sorted: $listOfLeaves VS ${listOfLeaves.sorted}")
        }
      }

      /*
     * 4. For each node w in T set h(w) to zero
     */
      for (node <- dfsNodes()) {
        hLCAcount(node) = 0
      }

      /*
     * 5. For each identifier i, compute the LCA of each consecutive pair of leaves
     * in Li and increment h(w) by one each time that w is the computed LCA
     */
      for {
        li <- listOfLeaves
        if li.size > 1
        pairOfLeaves <- li.sliding(2)
        pair1 = pairOfLeaves(0)
        pair2 = pairOfLeaves(1)
        lca = this.lca(pair1, pair2)
      } {
        hLCAcount(lca) = 1 + hLCAcount(lca)
      }

      /*
       * 6. With a bottom-up traversal of T, compute, for each node v, S(v) and
       * U(v)
       */
      computeSvAndUv(hLCAcount)

      _prepared_for_MCSA = true
    }
  }

  /**
    * Part of the MCSA pre-processing
    */
  protected def computeSvAndUv(hLCAcount: Array[Int]): Unit = {
    /**
      * Mapping: node dfs number -> number of leaves in the subtree
      *
      * The total number of leaves in the subtree of v.
      */
    val _S = Array.fill(numberOfNodes())(-1)
    // initialisation at -1 means "do not contains"
    object S {
      def apply(dfs: Int): Int =
        _S(dfs - 1)

      def update(dfs: Int, value: Int): Unit =
        _S(dfs - 1) = value

      def contains(dfs: Int): Boolean =
        _S(dfs - 1) != -1
    }

    /**
      * Mapping: node dfs number -> correction factor
      *
      * Correction factor which counts how many "duplicate" suffixes from the sequence
      * occur in v's subtree.
      */
    val _U = Array.fill(numberOfNodes())(-1)
    // initialisation at -1 means "do not contains"
    object U {
      def apply(dfs: Int): Int =
        _U(dfs - 1)

      def update(dfs: Int, value: Int): Unit =
        _U(dfs - 1) = value

      def contains(dfs: Int): Boolean =
        _U(dfs - 1) != -1
    }

    def alreadyComputed(nodeDFS: Int): Boolean =
      S.contains(nodeDFS) && U.contains(nodeDFS)

    @tailrec
    def bottomUpTraversal(candidates: mutable.Set[Int]): Unit = {
      if (candidates.nonEmpty) {
        val newCandidates = mutable.Set[Int]()
        for {
          v <- candidates
          realNode = dfsToNode(v)
        } {
          // Check that for each child v of the candidate node has a value of i(v)
          val children = realNode.children.map(nodeToDFS(_))
          val childrenComputed = children.forall {
            child => alreadyComputed(child)
          }
          if (childrenComputed) {
            // Computation of the number of leaves in the subtree
            S(v) = children.map { child => S(child) }.sum
            // Computation of the number of U(v)
            U(v) = hLCAcount(v) + children.map { child => U(child) }.sum
            // Computation of C(v)
            C(v) = S(v) - U(v)

            // Important operation to avoid recomputing a node
            // Remove from the next candidates the computed node
            newCandidates(v) = false

            // new candidate: parent node of v
            if (realNode != root) {
              val parentNode = realNode.asInstanceOf[NonRootNode].parent
              val parentNodeDFS = nodeToDFS(parentNode)
              if (!alreadyComputed(parentNodeDFS)) {
                // avoid adding an already computed candidate
                newCandidates(parentNodeDFS) = true
              } else {
                newCandidates(parentNodeDFS) = false
              }

            }
          } else {
            // children of the node have not been all computed
            // v stays a candidate node
            newCandidates(v) = true
          }
        }

        bottomUpTraversal(newCandidates)
      }
    }

    /*
     * For every leaf v:
     *  - S(v) = 1
     *  - U(v) = h(v)
     */
    for {
      leafNode <- leaves
      leaf = nodeToDFS(leafNode)
    } {
      S(leaf) = 1
      U(leaf) = hLCAcount(leaf)
      // Computation of C(v)
      C(leaf) = S(leaf) - U(leaf)
    }

    val parents = mutable.Set[Int]()
    for {
      leaf <- leaves
    } {
      val parent = nodeToDFS(leaf.parent)
      parents(parent) = true
    }
    bottomUpTraversal(parents)
  }

  def bulkMultipleCommonSubsequence(): Iterator[(Int, Repr)] = {
    // Mandatory pre-computation
    preprocessingMultipleCommonSubsequence()

    val currentParents = mutable.Stack[Node]()
    val currentChildrenStack = mutable.Stack[Iterator[NonRootNode]]()

    def currentChildren: Iterator[NonRootNode] =
      currentChildrenStack.head

    val parentsPathLabel = mutable.Stack[Repr]()

    def nbAppearances(node: NonRootNode): Int = {
      C(node)
    }

    def getChildren(parent: Node): Iterator[NonRootNode] = {
      parent.childrenMap
        .valuesIterator
        .withFilter {
          nbAppearances(_) > 1
        }
    }

    /*
     * Initialisation
     */
    currentParents.push(this.root)
    currentChildrenStack.push(getChildren(this.root))
    if (!currentChildrenStack.head.hasNext) {
      // Root has no child
      Iterator.empty

    } else {
      new Iterator[(Int, Repr)] {
        // Root has children
        var currentNode: Option[NonRootNode] = Some(currentChildren.next())

        @tailrec
        def computeNext(): Option[(Int, Repr)] = {
          debugging {
            val edgeLabel =
              currentNode match {
                case None =>
                  None
                case Some(node) =>
                  node.toDefiniteLabel(getSequenceLengthBy).value(getSubsequenceBy)
              }
            val n =
              currentNode match {
                case None =>
                  0
                case Some(node) =>
                  nbAppearances(node)
              }
            println(
              s"""
currentNode: $currentNode (edge-label: $edgeLabel, C=$n)
currentParents: ${currentParents.mkString(", ")}
reprs: ${parentsPathLabel.mkString(", ")}
""")
          }

          currentNode match {
            case Some(node) =>
              // 1- Computation of the result, if it exists
              val nbAppearance = nbAppearances(node)
              val result = if (nbAppearance > 1) {
                // Node is eligible, adding its path to the multiple common patterns
                val builder = if (parentsPathLabel.isEmpty) {
                  cbf()
                } else {
                  cbf() ++= parentsPathLabel.head // appending the parent path-label
                }
                builder ++= node.toDefiniteLabel(getSequenceLengthBy).value(getSubsequenceBy)
                val result = builder.result()

                val children = getChildren(node)
                if (children.hasNext) {
                  currentParents.push(node)
                  currentChildrenStack.push(children)
                  parentsPathLabel.push(result)
                }

                Some((nbAppearance, result))

              } else {
                None
              }

              // 2- Determination of the next node, if it exists
              if (currentChildren.hasNext) {
                // Pass to the next child
                currentNode = Some(currentChildren.next())

              } else {
                // No next child from this parent
                // Pop this parent, go to the next children if it exists
                currentParents.pop()
                currentChildrenStack.pop()
                if (parentsPathLabel.nonEmpty) parentsPathLabel.pop()

                // Up in the tree until there is a parent with an unvisited child
                while (currentChildrenStack.nonEmpty &&
                  currentParents.nonEmpty &&
                  !currentChildren.hasNext) {
                  currentParents.pop()
                  currentChildrenStack.pop()
                  if (parentsPathLabel.nonEmpty) parentsPathLabel.pop()
                }

                if (currentParents.isEmpty || currentChildrenStack.isEmpty) {
                  currentNode = None
                } else {
                  currentNode = Some(currentChildren.next)
                }
              }

              // Continue computing if needed
              result match {
                case r@Some(_) =>
                  debugging {
                    println(s"Result: $r")
                  }
                  r

                case None => // the currentNode was not eligible
                  debugging {
                    println("Non-eligible node, continue.")
                  }
                  computeNext()
              }

            case None => // no more next node
              None

          }
        }

        private var cachedResult = computeNext()

        def next(): (SequenceID, Repr) = {
          val result = cachedResult
          cachedResult = computeNext()
          result.get
        }

        def hasNext(): Boolean = {
          cachedResult.isDefined
        }
      }
    }
  }

  def multipleCommonSubsequence(): CommonSubsequences[Repr] = {
    // Mandatory pre-computation
    preprocessingMultipleCommonSubsequence()

    /*
     * Ordering of composite label by min length first (ascending order)
     * 
     * Ordering for the priority of the queue 
     *
     */
    val CompositeLabelOrdering = Ordering.by { l: CompositeLabel => l.length }
    val accumulator = mutable.Map[Int, mutable.PriorityQueue[CompositeLabel]]()
    val sizesImpl = mutable.Map[Int, Int]()
    val longestsImpl = mutable.Map[Int, Int]()
    for {
      node <- this.dfsInternalNodes()
      k = C(node)
      if k > 1
    } {
      if (!accumulator.contains(k)) {
        accumulator(k) = mutable.PriorityQueue[CompositeLabel]()(CompositeLabelOrdering)
        sizesImpl(k) = 0
        longestsImpl(k) = 0
      }
      val realNode = dfsToNode(node)
      if (realNode != root) {
        // Building label
        val labels = realNode.pathSingleLabel(getSequenceLengthBy)
        val label = CompositeLabel(labels)

        // Appending the label to the accumulator for size k
        accumulator(k).enqueue(label)

        // Counting the number of elements of size k 
        sizesImpl(k) += 1

        // Updating the longest element
        val longest = longestsImpl(k)
        if (label.length > longest) {
          longestsImpl(k) = label.length
        }
      }
    }

    type IteratorGenerator[Repr] = () => Iterator[Repr]

    def toIteratorHelper(l: Iterable[DefiniteLabel]): IteratorGenerator[Repr] =
      () =>
        new Iterator[Repr] {
          private var labels = l

          def hasNext: Boolean =
            labels.nonEmpty

          def next(): Repr = {
            val item = labels.head
            labels = labels.tail
            item.value(getSubsequenceBy)
          }
        }

    new CommonSubsequences[Repr] {
      private val acc = accumulator.mapValues { l => toIteratorHelper(l) }.toMap
      private val sizes = sizesImpl.toMap
      private val longests = longestsImpl.toMap

      def occurrences: Set[Int] = acc.keySet

      def apply(occ: Int): Iterator[Repr] =
        acc.getOrElse(occ, () => Iterator.empty)()

      def nb(occ: Int): Int =
        sizes.getOrElse(occ, 0)

      def longest(occ: Int): Int =
        longests.getOrElse(occ, 0)

    }
  }

  /*
   * GENERALIZED SUFFIX TREE BUILDER METHODS
   */
  // SEQUENCE RELATED
  private var _seqID: SequenceID = -1

  protected def generateSequenceID(): SequenceID = {
    _seqID += 1
    _seqID
  }

  def currentSequenceID(): SequenceID =
    _seqID

  def generateUniqueTerminalSymbol(): TerminalSymbol

  /**
    * Represents a sequence that may contain a terminal symbol
    *
    * @author Guillaume Dubuisson Duplessis
    *
    */
  sealed abstract class WrappedSequence {
    def apply(index: Int): Either[Alphabet, TerminalSymbol]

    def length: Int

    def isEmpty: Boolean

    def slice(i: Int, j: Int): Seq[Alphabet]

    def seq: Repr
  }

  object WrappedSequence {
    def apply(seq: Repr,
              uniqueTerminalSymbol: TerminalSymbol): WrappedSequence =
      WrappedSequenceWithTerminalSymbolImpl(seq, uniqueTerminalSymbol)

    def apply(seq: Repr): WrappedSequence =
      WrappedSequenceImpl(seq)

    case class WrappedSequenceImpl(seq: Repr) extends WrappedSequence {
      def apply(index: Int): Either[Alphabet, TerminalSymbol] = {
        Left(seq(index))
      }

      def length: Int =
        seq.length

      def isEmpty: Boolean =
        seq.isEmpty

      def slice(i: Int, j: Int): Seq[Alphabet] =
        seq.slice(i, j)

      override def toString = s"$seq"
    }

    case class WrappedSequenceWithTerminalSymbolImpl(
                                                      seq: Repr,
                                                      uniqueTerminalSymbol: TerminalSymbol) extends WrappedSequence {
      def apply(index: Int): Either[Alphabet, TerminalSymbol] = {
        if (index < seq.size) {
          Left(seq(index))
        } else if (index == seq.size) {
          Right(uniqueTerminalSymbol)
        } else {
          throw new NoSuchElementException()
        }
      }

      def length: Int =
        seq.length + 1

      def isEmpty: Boolean =
        seq.isEmpty

      def slice(i: Int, j: Int): Seq[Alphabet] =
        seq.slice(i, j)

      override def toString = s"$seq{$uniqueTerminalSymbol}"
    }

  }

  private val _sequences = mutable.ArrayBuffer[WrappedSequence]()

  def addSequence(realseq: Repr): (SequenceID, WrappedSequence) = {
    val currentSequenceID = generateSequenceID()

    val uniqueTerminalSymbol = generateUniqueTerminalSymbol()
    val seq = WrappedSequence(realseq, uniqueTerminalSymbol)
    _sequences.append(seq)
    debugging {
      println(s"Addition of terminal symbol: $seq")
    }

    (currentSequenceID, seq)
  }

  def getSequenceBy(id: SequenceID): Repr =
    _sequences(id).seq

  def getSequenceLengthBy(id: SequenceID): Int =
    _sequences(id).length

  def getSubsequenceBy(id: SequenceID, from: Int, until: Int): Repr = {
    val builder = cbf()
    builder ++= _sequences(id).slice(from, until)
    builder.result()
  }

  def dropSequenceBy(id: SequenceID, n: Int): Repr = {
    val builder = cbf()
    builder ++= _sequences(id).seq.drop(n)
    builder.result()
  }

  def getItemI(id: SequenceID, index: Int): Either[Alphabet, TerminalSymbol] =
    _sequences(id)(index)

  def getSequenceIDS(): Iterable[SequenceID] =
    _sequences.indices

  // LEAVES RELATED
  /**
    * List of leaves
    *
    * LCA pre-processing order leaves by their depth-first number.
    *
    */
  private var leaves = mutable.ArrayBuffer[LeafNode]()
  //mutable.PriorityQueue[LeafNode]()(Ordering.by[LeafNode, Int](leaf => nodeToDFS(leaf)))

  def nSuffixes(): Int =
  /*
   * Number of leaves - number of terminal characters that are 'technical' suffixes (e.g., '$')
   */
    leaves.size - size()

  // LABEL RELATED
  /**
    * Computes the path from the root to a given node.
    *
    * @note This method is costly in computational time (in other words, inefficient).
    *
    */
  protected def labelPartial(currentID: SequenceID, currentEnd: Int)(node: NonRootNode): Repr = {
    val builder = cbf()
    val edgeLabels = node
      // obtain the path label
      .pathLabel
      // get the values      
      .map(n => {
      if (n.sequenceID == currentID) {
        n.value[Alphabet, Repr](getSubsequenceBy, dropSequenceBy, Some(currentEnd))
      } else {
        n.value[Alphabet, Repr](getSubsequenceBy, dropSequenceBy)
      }
    })
    // concat the values
    for (edgeLabel <- edgeLabels) {
      builder ++= edgeLabel
    }

    builder.result
  }

  /**
    * Computes the edge label of a node taking into account the context of the Ukkonen's algorithm.
    *
    * @note This method is not efficient and should NOT be used because it makes copies of
    *       subsequences
    */
  protected def edgeLabelPartial(currentID: SequenceID, currentEnd: Int)(node: NonRootNode): Repr = {
    if (node.sequenceID == currentID) {
      node.value[Alphabet, Repr](getSubsequenceBy, dropSequenceBy, Some(currentEnd))
    } else {
      node.value[Alphabet, Repr](getSubsequenceBy, dropSequenceBy)
    }
  }

  protected def labelLengthPartial(currentID: SequenceID, currentEnd: Int)(node: NonRootNode): Int = {
    if (node.sequenceID == currentID) {
      node.length[Alphabet, Repr](getSequenceLengthBy, Some(currentEnd))
    } else {
      node.length[Alphabet, Repr](getSequenceLengthBy)
    }
  }

  protected def labels(currentID: SequenceID, currentEnd: Int)(node: NonRootNode): String = {
    val cv = if (_prepared_for_MCSA) {
      val nodeDFS = nodeToDFS(node)
      s"C(v)=${C(nodeDFS)}"
    } else {
      "???"
    }
    s"edge: ${edgeLabelPartial(currentID, currentEnd)(node)}; path: ${labelPartial(currentID, currentEnd)(node)}; $cv"
  }

  protected def start(node: NonRootNode): Either[Alphabet, TerminalSymbol] = {
    val startIndex = node.from - 1
    getItemI(node.sequenceID, startIndex)
  }

  /**
    * Retrieves the ith item on the edge label to the given node
    *
    * @note this method does not check for out-of-bounds error
    */
  protected def getItemLabel(node: NonRootNode, i: Int): Either[Alphabet, TerminalSymbol] = {
    val index = (node.from + i) - 1
    getItemI(node.sequenceID, index)
  }

  // INSERTION ALGORITHM
  def insert(preseq: Repr): Unit = {
    debugging {
      println(s"INSERTION: $preseq")
    }

    if (preseq.nonEmpty) {
      // Non-empty case

      this.toggleRequirePreprocessing() // pre-processings need to be recomputed

      // Addition of the sequence to the tree
      val (currentSequenceID, seq) = addSequence(preseq)
      val m = seq.length

      /*
       * GLOBAL STATE USED BY THE ALGORITHM
       */
      // Researching starting node
      val (startingNode, startingI, startingGap) =
      root.walkDownUntilUnmatch(seq, 0, seq.length - 1)(start, getItemLabel, labelLengthPartial(currentSequenceID, 0)) match {
        case Some((startingNode, nbCharacterLeft, gap)) =>
          /*
           * root                        startingNode
           *  + -------- + .... + ------ +
           *  |<------------------>|<--->|
           *        S[1..i]          gap
           *
           *  Here:
           *   i   = seq.length - nbCharacterLeft
           *
           */
          (startingNode, seq.length - nbCharacterLeft, gap)

        case None =>
          val leafNode = {
            countNewNode()
            root.addLeaf(
              currentSequenceID,
              start)
          }
          this.leaves.append(leafNode)
          (leafNode, 1, 0)
      }

      debugging {
        println(s"Research of the starting node: startingNode=$startingNode; i=$startingI; gap=$startingGap")
      }

      /*
       * Represents the node where the last extension S[j..i]S(i+1) has been
       * carried out.
       */
      var lastExtension: NonRootNode = startingNode
      var lastExtensionGap: Int = startingGap
      var (lastExtensionI, lastExtensionJ) = (1, 1)

      def saveLastExtension(
                             node: NonRootNode,
                             i: Int, j: Int,
                             gap: Int = 0): Unit = {
        lastExtension = node
        lastExtensionGap = gap
        lastExtensionI = i
        lastExtensionJ = j
      }

      /*
       * activeNode:       represents the node at (exactMatch == true) or 
       *                   below (exactMatch == false) S[j-1..i] in a phase
       * 
       */
      var (activeNode, exactMatch): (NonRootNode, Boolean) = (lastExtension, true)

      /*
       * activeNodeLabel:  represents the label of the edge conducting at activeNode
       *                   in extension j-1
       * 
       */
      object ActiveNodeLabel {
        var sequenceID = 0
        var from = 0
        var to = 0

        /**
          * Copy label information from a given node
          *
          */
        def copy(node: NonRootNode): Unit = {
          sequenceID = node.sequenceID
          from = node.from
          to = node.to
        }

        /**
          * Computes the definite label indices from the current sequence ID that is
          * being inserted and from the current end of this sequence
          */
        def labelIndices(currentID: SequenceID, currentEnd: Int): (Int, Int) = {
          val end = if (sequenceID == currentID) {
            if (to == END) {
              currentEnd
            } else {
              to
            }
          } else {
            if (to == END) {
              _sequences(sequenceID).length
            } else {
              to
            }
          }

          (from, end)
        }

      }
      ActiveNodeLabel.copy(lastExtension)

      def saveActiveNode(node: NonRootNode, exactMatchP: Boolean): Unit = {
        activeNode = node
        ActiveNodeLabel.copy(node)
        exactMatch = exactMatchP
      }

      // Node representing the previously created internal node in the previous extension j-1
      var w: Option[InternalNode] = None

      def resetLastInternalNode(): Unit = {
        w = None
      }

      def saveLastInternalNode(node: InternalNode): Unit = {
        w = Some(node)
      }

      def buildSuffixLink(currentEnd: Int)(to: Node): Unit = {
        if (w.isDefined) {
          val Some(internalNode) = w
          debugging {
            println(s"\tCreation of a suffix link (${labels(currentSequenceID, currentEnd)(internalNode)} -> $to)")
          }
          internalNode.suffix_link = Some(to)
        } else {
          debugging {
            println(s"\tNo suffix link to create (w=$w) to: $to")
          }
        }
        w = None
      }

      /*
       * HELPER METHODS USED BY THE ALGORITHM
       */
      def findNodeToExtend(i: Int, j: Int, ji: Int): (Node, Int) =
        if (j == ji) {
          // First extension trick: we start at the last extension operated on the previous phase meaning that
          //                        the previous phase added S[j*..i] and here we add S[j*..i+1]
          //                        in other words, the repeated extension of j* [...] can execute the suffix 
          //                        extension rule for j* without any up-walking, suffix link traversals, or 
          //                        node skipping
          //
          (lastExtension, lastExtensionGap) // returning S[j*..i]
          // Note: rule 1 will we automatically applied to this node

        } else {
          // Extension for j > j* (up-walking, suffix link traversals, and node skipping)
          //
          // Find the first node v at or above the end of S[j-1..i] that either has a suffix link from
          // it or is the root.
          // Here, S[j-1..i] is represented by 'activeNode'
          //
          // Up-walking process if needed
          val v =
          if (exactMatch) {
            // S[j-1..i] is at the 'activeNode'
            if (activeNode.has_suffix_link) {
              activeNode
            } else {
              activeNode.parent
            }
          } else {
            // S[j-1..i] is above 'activeNode' (i.e. it is a prefix of the path label of 'activeNode')
            activeNode.parent
          }
          debugging {
            println(s"\tFinding the first node v at or above S[${j - 1}..$i]: $v")
          }

          // Let y (possibly empty) denote the string between v and the end of S[j-1..i]
          val yIsActiveNode = if (v == activeNode)
            true
          else
            false

          /*
           * Research process using either:
           *  - naive research algorithm, or
           *  - suffix link traversals and node skipping
           */
          v match {
            // Standard search (root has no suffix link)
            // If v is the root, then follow the path S[j..i] from the root (as in the naive algorithm).
            case Root(_) =>
              // recover S[j..i] 
              root.findPathEndContaining(seq, j - 1, i - 1)(start, getItemLabel, labelLengthPartial(currentSequenceID, i)) match {
                // Special case: Extension i + 1 of phase i + 1 extends the empty suffix of S[1..i],
                // that is, it puts the single character string S(i+1) into the tree (unless it is 
                // already there).
                case None if j == (i + 1) =>
                  (root, 0)

                case None =>
                  debugging {
                    println(root.mkString(labels(currentSequenceID, i)))
                  }
                  val sji = seq.slice(j - 1, i) // recover S[j..i] 
                  throw new Error(s"Unable to find the following path in the tree: $sji")

                case Some((node, gap)) =>
                  debugging {
                    println(s"\tpath found with gap: $gap")
                  }
                  // Suffix extension rules
                  (node, gap)
              }

            // Suffix link search
            // If v is not the root, traverse the suffix link from v to node s(v) and then walk
            // from s(v) following the path for string y.
            case node: NonRootNode =>
              val sv = node.suffix_link.get // suffix link traversal
              debugging {
                println(s"\tFollowing suffix link from $node to $sv")
              }

              if (yIsActiveNode) {
                debugging {
                  println(s"\tSkipping soft search from: $sv (label y is empty)")
                }
                (sv, 0)

              } else {
                val (startIndex, endIndex) = ActiveNodeLabel.labelIndices(currentSequenceID, i)
                val sequenceY = _sequences(ActiveNodeLabel.sequenceID)

                sv.walkDown(sequenceY, startIndex - 1, endIndex - 1, i)(start, labelLengthPartial(currentSequenceID, i))
              }

          }
        }

      /*
       * Applies extension rules to the given node knowing the gap between the end of
       * S[j..i] and 'foundNode'
       * @param foundNode node at or below S[j..i]
       * @param pathGap gap between the end of S[j..i] and the path label of 'foundNode'
       *                (0 means S[j..i] is the path label of 'foundNode',
       *                 > 0 means S[j..i] is contained in the path label
       *                 of 'foundNode')
       * @param j extension j
       * @param i phase i
       * @return true if extension has been stop by encountering rule 3, 
       *         else false
       */
      def extendNode(foundNode: Node, pathGap: Int, j: Int, i: Int): Boolean = {
        var rule3ShowStopper = false
        val siplus1 = seq(i) // retrieve the character S(i+1)

        foundNode match {
          case Root(_) =>
            // Special case of inserting only S(i+1)
            // i.e. when j = i + 1 > i: S[j..i] is empty
            // In that case, we do not save currentPreviousSji which will be
            // reset in next extension phase j = 1 of (i+2)
            // 
            root.childStartingWith(siplus1) match {
              case None =>
                // Rule 2
                debugging {
                  println("RULE 2")
                  println(s"\tThere is not a child starting with $siplus1")
                }

                val leafNode = {
                  countNewNode()

                  new LeafNode(
                    parent = root,
                    sequenceID = currentSequenceID,
                    from = i + 1,
                    j)
                }
                this.leaves.append(leafNode)
                root.add(leafNode, start)

                saveLastExtension(leafNode, i, j)
                saveActiveNode(leafNode, exactMatchP = false)

              case Some(n) =>
                debugging {
                  println("RULE 3")
                  println(s"\tThere IS a child starting with $siplus1: $n")
                }
                // Rule 3: we do nothing
                rule3ShowStopper = true
            }
            // Creation of suffix link
            buildSuffixLink(i)(root)

          case workingNode: NonRootNode =>
            // Using the extension rules, ensure that the string S[j..i]S(i+1) is in the tree.
            // Path label:
            //                                   foundNode
            // + ----+ [...] + ---- . ---------- +
            //               .      ^            .
            // j             .      i <-pathGap->.
            //               .                   .
            //               .<-foundNode.label->.
            //
            // Size constraint:
            //  0 <= pathGap < foundNode.label.size
            //
            val perfectMatchWithFoundNode = (pathGap == 0)

            if (perfectMatchWithFoundNode) {
              // Path S[j..i] ends at a node

              workingNode match {
                case leaf@LeafNode(_, _, _, _, _) =>
                  debugging {
                    println("RULE 1")
                  }
                  // Rule 1: character S(i+1) is added to the end of the label on that leaf edge
                  // Saving "j-1" variables

                  // save the current leaf pointing to S[j..i]
                  saveLastExtension(leaf, i, j)
                  // save the current leaf pointing to S[j..i]
                  saveActiveNode(leaf, exactMatchP = false)

                  resetLastInternalNode()

                case other@InternalNode(_, _, _, _, _, _) =>
                  // Find the child node which edge label starts with S(i+1)
                  saveActiveNode(other, exactMatchP = true)

                  other.childStartingWith(siplus1) match {
                    case None =>
                      debugging {
                        println("RULE 2 (perfect match)")
                      }
                      // Rule 2: a new leaf edge starting from the end of the path must be created
                      //         and labeled with character S(i+1)   
                      val leafNode = {
                        countNewNode()

                        new LeafNode(
                          parent = other,
                          sequenceID = currentSequenceID,
                          from = i + 1,
                          j)
                      }
                      this.leaves.append(leafNode)
                      saveLastExtension(leafNode, i, j) // save the current leaf pointing to S[j..i]
                      other.add(leafNode, start)

                    case Some(nonRootNode) =>
                      debugging {
                        println("RULE 3")
                      }
                      // Rule 3: we do nothing
                      rule3ShowStopper = true
                  }

                  // Creation of suffix link
                  buildSuffixLink(i)(other)

                  resetLastInternalNode()
              }
            } else {
              // Path S[j..i] ends in the middle of an edge

              debugging {
                println(s"\tPath S[j..i] ends in the middle of an edge")
              }
              // Path label:
              //                                   workingNode
              // + ----+ [...] + ---- . ---------- +
              //               .      ^            .
              // j             .      i <-pathGap->.
              //               .                   .
              //               .<-workingNode.label->.
              //
              // Size constraint:
              //  0 <= pathGap < workingNode.label.size
              //

              val labelIndexOfItemAfterSji = workingNode.from + labelLengthPartial(currentSequenceID, i)(workingNode) - pathGap
              // -1 below is because index in label starts from 1 while it starts from 0 in sequence
              val seqIndexOfItemAfterSji = labelIndexOfItemAfterSji - 1
              val nextItemAfterSji = getItemI(workingNode.sequenceID, seqIndexOfItemAfterSji)

              if (nextItemAfterSji == siplus1) {
                // Rule 3: we do nothing
                debugging {
                  println(s"\tWorking node: $workingNode")
                  println(s"\t$nextItemAfterSji == $siplus1")
                  println("RULE 3")
                  println(s"\tRule 3: we do nothing")
                }

                // Creation of suffix link
                buildSuffixLink(i)(workingNode)

                rule3ShowStopper = true
                resetLastInternalNode()

              } else {
                debugging {
                  println("RULE 2 (imperfect match)")
                }
                // Rule 2: a new leaf edge starting from the end of S[j..i] must be created and
                //         labeled with character S(i+1). A new node will also have to be created
                //         there if S[j..i] ends inside an edge.
                val parentNode = workingNode.parent

                val newInternalNode = {
                  countNewNode()

                  new InternalNode(// Internal node which path is S[j..i]
                    parent = parentNode,
                    childrenMap = emptyNodes(),
                    sequenceID = workingNode.sequenceID,
                    from = workingNode.from, // Note the particular case of the root: Label(j, i)
                    to = labelIndexOfItemAfterSji - 1)
                }
                // Changing node
                // - changing parent
                parentNode.remove(workingNode, start) // Removing from parent child
                parentNode.add(newInternalNode, start)

                // Modifying working node
                workingNode.parent = newInternalNode
                // - changing label
                workingNode.from = labelIndexOfItemAfterSji
                newInternalNode.add(workingNode, start)
                // Note the particular case of the root: newLabel.length(i) is:
                // (i - j) + 1 (the length of the newLabel)

                debugging {
                  println(s"\tCreation of a new internal node: $newInternalNode")
                }

                // Creation of suffix link
                buildSuffixLink(i)(newInternalNode)

                // Saving S[j..i]
                saveActiveNode(newInternalNode, exactMatchP = true)

                // Adding new leaf node
                val leafNode = {
                  countNewNode()

                  new LeafNode(
                    parent = newInternalNode,
                    sequenceID = currentSequenceID,
                    from = i + 1,
                    j)
                }
                this.leaves.append(leafNode)
                saveLastExtension(leafNode, i, j)

                newInternalNode.add(leafNode, start)

                // If a new internal node w was created in extension j-1 (by extension rule 2), then by Lemma 6.1.1,
                // string alpha must end at node s(w), the end node for the suffix link from w.
                // Create the suffix link (w, s(w)) from w to s(w).
                saveLastInternalNode(newInternalNode)
              }
            }
        }
        rule3ShowStopper
      }

      /*
       * CORE ALGORITHM
       */
      // ji denotes the index j of the last extension in the previous phase
      var ji = 1
      var rule3ShowStopper = false

      var i = startingI
      var j = 1
      while (i <= (m - 1)) {
        // Phase i + 1
        j = ji // starting at the place where the last extension was carried out
        rule3ShowStopper = false

        while (j <= (i + 1) && !rule3ShowStopper) {
          // Explicit extensions
          debugging {
            // Setting up some useful variables
            val sji = seq.slice(j - 1, i)
            // recover S[j..i]
            val siplus1 = seq(i) // retrieve the character S(i+1)
            println(s"Phase ${i + 1}; extension $j")
            println(s"\tS[j..i] = S[$j..$i]: $sji")
            println(s"\tInsertion of S(${i + 1}): $siplus1")
            println(s"\tLast extension S[j..i]=S[$lastExtensionI..$lastExtensionJ]: $lastExtension")
            println(s"\tNode at or below S[j-1..i]=S[${j - 1}..$i]: $activeNode")
          }

          // Researching the extension node
          val (foundNode, pathGap) = findNodeToExtend(i, j, ji)

          // Application of extension rules
          rule3ShowStopper = extendNode(foundNode, pathGap, j, i)

          j += 1 // so that j is jStar
        }
        // end of phase i+1

        debugging {
          if (rule3ShowStopper) {
            println(s"RULE 3 SHOW STOPPER: i=$i j=$j")
          }
          println(root.mkString(labels(currentSequenceID, i + 1)))
        }

        // update of ji so that we start where the last explicit extension
        // has been computed
        if (rule3ShowStopper) {
          ji = j - 2
          debugging {
            println(s"END OF PHASE BY RULE 3 (ji=$ji)")
          }
        } else {
          // else it means that the last extension was with another rule
          // than rule 3
          ji = j - 1
          debugging {
            println(s"END OF PHASE NOT BY R3 (ji=$ji)")
          }
        }

        i += 1
      }
      debugging {
        println(s"FINAL TREE (i=$i):")
        println(root.mkString(labels(currentSequenceID, i)))
      }
    }
  }

  def mkString: String =
    root.mkString(labels)
}

object GeneralizedSuffixTreeBuilder {
  type Generator[T] = Iterator[T]
  type GeneratorProvider[T] = () => Generator[T]
  type SequenceID = Int
  type Sequence[T] = Seq[T]
  type TerminalSymbol = Int

  /*
   * IMPLICIT GENERATOR PROVIDERS
   */
  protected def uniqueTerminalSymbolGeneratorInt(): Generator[Int] =
    new Iterator[Int] {
      private var i = 0

      def hasNext(): Boolean = true

      def next(): SequenceID = {
        i -= 1
        i
      }
    }

  /**
    * Creates an empty generalized suffix tree builder
    *
    * @param icbf
    * @tparam Alphabet type of the items of the sequences
    * @tparam Repr     type of the sequences
    * @return a new empty generalized suffix tree builder
    */
  protected[gstlib] def empty[Alphabet, Repr <% Sequence[Alphabet]]()(
    implicit icbf: CanBuildFrom[Repr, Alphabet, Repr]): GeneralizedSuffixTreeBuilder[Alphabet, Repr] =
    new GeneralizedSuffixTreeBuilder[Alphabet, Repr]() {
      val cbf: CanBuildFrom[Repr, Alphabet, Repr] = icbf
      val generator: Generator[SequenceID] = uniqueTerminalSymbolGeneratorInt()

      def generateUniqueTerminalSymbol(): SequenceID = generator.next()
    }

  val DEBUG = false

  def debugging(code: => Unit): Unit = {
    if (DEBUG) {
      code
    }
  }
}