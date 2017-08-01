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
import scala.collection.mutable
import GeneralizedSuffixTreeBuilder.{ Sequence, SequenceID, TerminalSymbol, debugging }
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

protected[gstlib] object InnerTree {
  val END: SequenceID = -1
  type Nodes = mutable.Map[Any, NonRootNode]
  def emptyNodes[Repr](): Nodes =
    mutable.OpenHashMap[Any, NonRootNode]()

  /**
   * Represents a walkable sequence of the inner tree algorithms
   */
  type Walkable[T] = {
    def apply(int: Int): T
    def isEmpty: Boolean
  }

  /**
   * Represents an immutable definite label
   * @author Guillaume Dubuisson Duplessis
   *
   */
  sealed abstract class DefiniteLabel {
    def value[Alphabet, Repr <% Sequence[Alphabet]](
      getSubsequence: ((SequenceID, Int, Int) => Repr))(implicit cbf: CanBuildFrom[Repr, Alphabet, Repr]): Repr
    def length: Int
  }

  case class SingleLabel(
      sequenceID: SequenceID,
      from: Int,
      to: Int) extends DefiniteLabel {
    def value[Alphabet, Repr <% Sequence[Alphabet]](
      getSubsequence: ((SequenceID, Int, Int) => Repr))(implicit cbf: CanBuildFrom[Repr, Alphabet, Repr]): Repr =
      getSubsequence(sequenceID, from - 1, to)

    def length: Int = (to - from + 1)

    override def toString =
      s"(sid=$sequenceID,[$from;$to])"
  }

  case class CompositeLabel(
      labels: List[SingleLabel]) extends DefiniteLabel {
    def value[Alphabet, Repr <% Sequence[Alphabet]](
      getSubsequence: ((SequenceID, Int, Int) => Repr))(implicit cbf: CanBuildFrom[Repr, Alphabet, Repr]): Repr = {
      val builder = cbf()
      for (label <- labels) {
        builder ++= label.value[Alphabet, Repr](getSubsequence)
      }
      builder.result
    }

    val length: Int = labels.foldLeft(0)((sum, label) => sum + label.length)

    override def toString =
      s"CompositeLabel(nb=${labels.size}, length=$length)"
  }

  /**
   * A node of a tree.
   *
   * @author Guillaume Dubuisson Duplessis
   *
   */
  sealed abstract class Node {
    /**
     * Depth-first number
     */
    var dfs: Int = -1

    def childrenMap: Nodes
    def children: List[NonRootNode] =
      childrenMap.values.toList
    def parents(): List[NonRootNode]

    def size(): Int

    /**
     * Finds the path end following a sequence (given as a global sequence, the starting
     * and end indexes of the subsequence to search) from the root node if it exists
     * @param s the global sequence that contains the searched subsequence
     * @param startIndex the start index in the global sequence of the first character of the
     *                   searched subsequence
     * @param endIndex the end index in the global sequence of the last character of the
     *                 searched subsequence
     * @param start a function that given a NonRootNode retrieves efficiently the first item of
     *              the edge-label conducting to the given node in the tree
     * @param getEdgeLabelItem a function that retrieves efficiently the ith item in the edge label
     *                         to the given node
     * @param labelLength a function that given a NonRootNode computes the length of the edge-label
     *                    conducting to this node in the tree
     * @return an optional pair containing the NonRootNode which path-label includes the searched
     *         subsequence along with the number of item that are in the path-label and not in the
     *         searched subsequence (i.e., the gap between the end of the search subsequence in the
     *         path-label to reach the node)
     */
    def findPathEndContaining[Alphabet](
      s: Walkable[Either[Alphabet, TerminalSymbol]],
      startIndex: Int,
      endIndex: Int)(
        start: NonRootNode => Either[Alphabet, TerminalSymbol],
        getEdgeLabelItem: (NonRootNode, Int) => Either[Alphabet, TerminalSymbol],
        labelLength: NonRootNode => Int): Option[(NonRootNode, Int)] = {
      if (s.isEmpty || (endIndex < startIndex)) {
        None
      } else {
        this.walkDownUntilUnmatch(s, startIndex, endIndex)(start, getEdgeLabelItem, labelLength) match {
          case Some((node, nbCharacterLeft, gap)) if nbCharacterLeft == 0 =>
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
            Some((node, gap))

          case _ =>
            None
        }
      }
    }

    /**
     * Computes the child node of this node which edge-label starts with the given item
     */
    def childStartingWith[Alphabet](item: Either[Alphabet, TerminalSymbol]): Option[NonRootNode] = {
      val value = item.merge // important line to obtain the actual value of the first item of the edge-label
      childrenMap.get(value)
    }

    /**
     * Computes the path label as a list of NonRootNode
     *
     * Beware, labels of these nodes might include special upper limits
     * such as the END constant.
     */
    def pathLabel: List[NonRootNode]

    /**
     * Computes the path label as a list of definitive labels given a function to get
     * the length of a sequence from its ID
     *
     */
    def pathSingleLabel(getSequenceLengthBy: SequenceID => Int): List[SingleLabel]

    def add[Alphabet](child: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit
    def remove[Alphabet](child: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit

    protected def leavesHelper(
      currentNode: Node): List[LeafNode] =
      currentNode match {
        case leaf @ LeafNode(_, _, _, _, _) =>
          List(leaf)

        case other =>
          other.children.foldLeft(List[LeafNode]()) {
            case (currentLeaves, child) =>
              currentLeaves ::: leavesHelper(child)
          }
      }

    def leaves(): List[LeafNode] =
      leavesHelper(this)

    /**
     * Walks down the tree following the sequence until a mismatch occurs
     *
     * It dissociates the following cases:
     *  1. the sequence is not in the tree, even partially (returns None)
     *  2. the sequence is partially in the tree...
     *     a. ... and it stops on a non-root node (returns Some(node, int > 0, 0))
     *     b. ... and it stops in a middle of an edge (returns Some(node, int > 0, int > 0))
     *  3. the sequence is entirely in the tree...
     *     a. ... and it stops on a non-root node (returns Some(node, 0, 0))
     *     b. ... and it stops in a middle of an edge (returns Some(node, 0, int > 0))
     *
     * @param s the searched sequence
     * @param startIndex the first index of search in the sequence (inclusive)
     * @param endIndex the last index of search in the sequence (inclusive)
     * @param start retrieves efficiently the first item in the edge label to the given node
     * @param getEdgeLabelItem retrieves efficiently the ith item in the edge label to the given node
     * @param labelLength computes efficiently the length of the edge label to the given node
     * @return None if the sequence is not partially in the tree, else a triple
     *         containing 1) the non-root node at or below the unmatch or the
     *         end of the sequence happened, 2) the number of character left in
     *         the given sequence and 3) the number of character left to reach the
     *         non-root node (0 means "perfect match")
     */
    def walkDownUntilUnmatch[Alphabet](
      s: Walkable[Either[Alphabet, TerminalSymbol]],
      startIndex: Int,
      endIndex: Int)(
        start: NonRootNode => Either[Alphabet, TerminalSymbol],
        getEdgeLabelItem: (NonRootNode, Int) => Either[Alphabet, TerminalSymbol],
        labelLength: NonRootNode => Int): Option[(NonRootNode, Int, Int)]

    /**
     * Walks down from this node following the given sequence
     * @param s the searched sequence
     * @param startIndex the first index of search in the sequence (inclusive)
     * @param endIndex the last index of search in the sequence (inclusive)
     * @param currentEnd current index of the end (i in suffix S[j..i] of phase i)
     * @param start computes the first item of a label
     * @return a pair (node, length) containing the node which path label contains sequence s, and
     *         the number of items left in the path to arrive to the node (0 means a perfect match)
     * @note walkDown is overriden by 'NonRootNode'
     */
    def walkDown[Alphabet](
      s: Walkable[Either[Alphabet, TerminalSymbol]],
      startIndex: Int,
      endIndex: Int,
      currentEnd: Int)(
        start: NonRootNode => Either[Alphabet, TerminalSymbol],
        labelLength: NonRootNode => Int): (NonRootNode, Int) = {

      val sLength = (endIndex - startIndex + 1)
      val sIndexMax = endIndex

      @tailrec
      def walkDownHelper(
        currentNode: NonRootNode,
        sStartIndex: Int, currentEnd: Int): (NonRootNode, Int) = {
        if (sStartIndex > sIndexMax) {
          // This node is a perfect match for the searched indexed seq
          (currentNode, 0)
        } else {
          // Determination of the right child (starting with the same item)
          val startingItem = s(sStartIndex)
          /*
           * s      : a  b  c  d
           * indexes: 0  1  2  3
           *             ^
           *        sStartIndex
           *     
           * Length: s.length - sStartIndex
           *           4      -      1
           */
          val seqSize = sIndexMax - sStartIndex + 1

          debugging {
            println(s"\tFinding child starting with: $startingItem from node: $currentNode")
          }
          val child = currentNode.childStartingWith(startingItem).get
          val size = labelLength(child)
          debugging {
            println(s"\tFound child: $child (seqSize=$seqSize;edge label size: $size)")
          }
          // Skip/count trick: moving from one node to another in constant time, considering that:
          //  - knowing the number of items on edges in constant time,
          //  - extracting a given item in constant time.
          //      
          if (seqSize < size) {
            // The sequence ends between this node and its child
            (child, size - seqSize)
          } else {
            walkDownHelper(child, sStartIndex + size, currentEnd)
          }
        }
      }

      if (s.isEmpty) {
        // this cannot happen, soft search require a non-empty sequence from the root
        throw new UnsupportedOperationException("Impossible to soft search the suffix tree with an empty sequence")
      } else {
        // Determination of the right child (starting with the same item)
        val startingItem = s(startIndex)
        val child = this.childStartingWith(startingItem).get
        val size = labelLength(child)
        // Skip/count trick
        if (sLength >= size) {
          walkDownHelper(child, startIndex + size, currentEnd)
        } else {
          // The sequence ends between this node and its child
          (child, size - sLength)
        }

      }
    }
  }

  /**
   * Represents a non-root node in the tree that contains a label
   *
   * A NonRootNode includes the edge-label compression schema representing a
   * substring of a given sequence by a beginning and an end positions (inclusive).
   *
   * Labels start from 1. For a sequence s of m elements, indexes range from 1
   * to m (inclusive).
   *
   * A label can be specified only by its start index ('from') which means that
   * it ranges from the 'from' index to the end of the current sequence.
   *
   */
  sealed abstract class NonRootNode extends Node {
    def parent: Node
    def parent_=(n: Node): Unit
    def parents(): List[NonRootNode] =
      parentsHelper(this.parent, List(this))

    def sequenceID: SequenceID
    def sequenceID_=(id: SequenceID): Unit
    def from: Int
    def from_=(from: Int): Unit
    def to: Int
    def to_=(to: Int): Unit

    def labelToString: String =
      if (to == END) {
        s"(sid=$sequenceID,[$from;END])"
      } else {
        s"(sid=$sequenceID,[$from;$to])"
      }

    /**
     * Computes the value of this label
     * @param currentEnd optional end index (if the end is not the real end)
     */
    def value[Alphabet, Repr <% Sequence[Alphabet]](
      sliceSequenceBy: ((SequenceID, Int, Int) => Repr),
      dropSequenceBy: ((SequenceID, Int) => Repr),
      currentEnd: Option[Int] = None): Repr =
      currentEnd match {
        case Some(endIndex) =>
          if (to == END) {
            sliceSequenceBy(sequenceID, from - 1, endIndex)
          } else {
            sliceSequenceBy(sequenceID, from - 1, to)
          }

        case None =>
          if (to == END) {
            /*
           * Take all elements from index 'from' to the end, i.e.:
           * + -- + ..... + ------- + .... +
           * 1    2    from-1     from     to    (label indexes)
           * 0    1    from-2     from-1   to-1  (seq indexes)
           * | <--------> |
           *    to drop = (from - 1) elements
           *    
           */
            dropSequenceBy(sequenceID, from - 1)
          } else {
            sliceSequenceBy(sequenceID, from - 1, to)
          }
      }

    /**
     * Computes the length of this label
     *
     * @param currentEnd an option including the current index of the end of the sequence, or None if
     *        the full sequence should be considered
     * @return the length of this label
     */
    def length[Alphabet, Repr <% Sequence[Alphabet]](
      getSequenceLength: (SequenceID => Int),
      currentEnd: Option[Int] = None): Int =
      currentEnd match {
        case Some(currentEnd) =>
          if (to == END) {
            (currentEnd - from + 1)
          } else {
            (to - from + 1)
          }

        case None =>
          if (to == END) {
            (getSequenceLength(sequenceID) - from + 1)
          } else {
            (to - from + 1)
          }
      }

    def toDefiniteLabel(getSequenceLength: (SequenceID => Int)): SingleLabel = {
      val definiteTo =
        if (to == END) {
          getSequenceLength(sequenceID)
        } else {
          to
        }

      SingleLabel(sequenceID, from, definiteTo)
    }

    @tailrec
    protected final def parentsHelper(
      currentNode: Node,
      acc: List[NonRootNode]): List[NonRootNode] =
      currentNode match {
        case Root(_) =>
          acc
        case nonRoot: NonRootNode =>
          parentsHelper(nonRoot.parent, nonRoot :: acc)
      }

    def suffix_link: Option[Node]
    def has_suffix_link: Boolean = suffix_link.isDefined

    def pathLabel: List[NonRootNode] =
      pathLabelHelper(this.parent, List(this))

    @tailrec
    protected final def pathLabelHelper(
      currentNode: Node,
      acc: List[NonRootNode]): List[NonRootNode] =
      currentNode match {
        case Root(_) =>
          acc
        case nonRoot: NonRootNode =>
          pathLabelHelper(nonRoot.parent, nonRoot :: acc)
      }

    def pathSingleLabel(getSequenceLengthBy: SequenceID => Int): List[SingleLabel] = {
      @tailrec
      def pathSingleLabelHelper(
        currentNode: Node,
        acc: List[SingleLabel]): List[SingleLabel] =
        currentNode match {
          case Root(_) =>
            acc
          case nonRoot: NonRootNode =>
            pathSingleLabelHelper(nonRoot.parent,
              nonRoot.toDefiniteLabel(getSequenceLengthBy) :: acc)
        }

      pathSingleLabelHelper(this.parent, List(this.toDefiniteLabel(getSequenceLengthBy)))
    }

    final def walkDownUntilUnmatch[Alphabet](
      s: Walkable[Either[Alphabet, TerminalSymbol]],
      startIndex: Int,
      endIndex: Int)(
        start: NonRootNode => Either[Alphabet, TerminalSymbol],
        getEdgeLabelItem: (NonRootNode, Int) => Either[Alphabet, TerminalSymbol],
        labelLength: NonRootNode => Int): Option[(NonRootNode, Int, Int)] = {
      val sIndexMax = endIndex

      @tailrec
      def walkDownUntilUnmatchHelper(currentNode: NonRootNode, sStartIndex: Int): Option[(NonRootNode, Int, Int)] = {
        val edgeLabelLength = labelLength(currentNode)

        // Removal of character of the sequence following the edge-label
        var indexItem = 0
        while ( // Ensuring indexes limits
        indexItem < edgeLabelLength && (sStartIndex + indexItem) <= sIndexMax
          // comparing item in the sequence and the label
          && s(sStartIndex + indexItem) == getEdgeLabelItem(currentNode, indexItem)) {
          indexItem += 1
        }

        val newSeqStartIndex = sStartIndex + indexItem
        val seqIsExhausted = (newSeqStartIndex > sIndexMax)
        val nbItemsLeftInTheSequence = if (seqIsExhausted) {
          0
        } else {
          sIndexMax - newSeqStartIndex + 1
        }

        /*
       * Several cases are possible:
       * 1) the rest sequence is empty (meaning that the sequence is implicitly in the tree)
       * 2) the rest sequence is not empty...
       *    a) ... and the edge-label is completely contained in the sequence
       *       (i.e., we must continue down the tree)
       *    b) ... and the edge-label is not fully contained in the sequence
       *       (i.e., we should stop)
       *    
       */
        val numberOfTakenItem = newSeqStartIndex - sStartIndex
        debugging {
          println(s"Number of item taken in the sequence: $numberOfTakenItem")
          println(s"Number of item left in the sequence: $nbItemsLeftInTheSequence")
        }
        val gap = edgeLabelLength - numberOfTakenItem

        if (seqIsExhausted) {
          // Case 1
          Some(currentNode, 0, gap)

        } else {
          if (numberOfTakenItem == edgeLabelLength) {
            // Case 2)a)
            val first = s(newSeqStartIndex)

            currentNode.childStartingWith(first) match {
              case Some(nonRootNode) =>
                // it is possible to continue down the tree
                debugging {
                  println(s"Continue to go down to: $nonRootNode")
                }
                walkDownUntilUnmatchHelper(nonRootNode, newSeqStartIndex)

              case None =>
                // it is not possible to continue down the tree because it does not
                // exist a children which edge-label starts with the first character
                // of the newSequence
                // 
                Some(currentNode, nbItemsLeftInTheSequence, 0) // currentNode is a perfect match
            }
          } else {
            // Case 2)b)
            Some(currentNode, nbItemsLeftInTheSequence, gap)
          }
        }
      }

      walkDownUntilUnmatchHelper(this, startIndex)
    }

    def mkString[Alphabet](
      label: NonRootNode => String,
      tabuling: String): String =
      s"$tabuling$toString | label: ${label(this)}" +
        (for (child <- children) yield child.mkString(label, s"$tabuling  + ")).mkString("\n", "\n", "")

  }

  final class InternalNode(
      var parent: Node,
      val childrenMap: Nodes,
      // label value
      var sequenceID: SequenceID,
      var from: Int,
      var to: Int = END,
      var suffix_link: Option[Node] = None) extends NonRootNode {

    def size(): Int =
      1 + children.map { _.size() }.sum

    def add[Alphabet](child: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit = {
      //require(!childrenMap.contains(first))
      val value = start(child).merge // important line to obtain the actual value of the first item of the edge-label
      childrenMap(value) = child
    }

    def remove[Alphabet](node: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit = {
      //require(childrenMap.contains(first))
      this.childrenMap.remove(start(node))
    }

    override def toString =
      s"InternalNode($labelToString;dfs=${this.dfs})"

    // Overriding equals and hashcode to avoid cyclic comparisons of parent node
    override def equals(other: Any): Boolean =
      other match {
        case n: InternalNode =>
          this eq n // reference equality

        case _ => false
      }

    // Reminder of the hashcode contract: "equal objects must have equal hash codes"
    override def hashCode: Int =
      41 * (41 * (41 + this.sequenceID) + this.from) + this.to
  }

  object InternalNode {
    def unapply(node: InternalNode): Option[(Node, Nodes, SequenceID, Int, Int, Option[Node])] = {
      Some((node.parent, node.childrenMap, node.sequenceID, node.from, node.to, node.suffix_link))
    }
  }

  /**
   * Leaf node in the tree.
   *
   * A leaf node is uniquely identified by its label and its number 'n' representing
   * the position of the edge-path to this leaf in the sequenceID.
   * @author Guillaume Dubuisson Duplessis
   *
   */
  final class LeafNode(
      var parent: Node,
      // label value
      var sequenceID: SequenceID,
      var from: Int,
      val n: Int,
      var suffix_link: Option[Node] = None) extends NonRootNode {

    def childrenMap: Nodes =
      throw new UnsupportedOperationException("Leaf node does not have children")
    override def children: List[NonRootNode] = List()

    def to: Int = END
    def to_=(to: Int): Unit =
      throw new UnsupportedOperationException("Cannot modify the 'to' field of a LeafNode")

    def size(): Int =
      1

    override def toString =
      s"Leaf($n, $labelToString)"

    def add[Alphabet](node: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit =
      throw new UnsupportedOperationException("Impossible to add a child node to a leaf")
    def remove[Alphabet](node: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit =
      throw new UnsupportedOperationException("Impossible to remove a child node from a leaf")

    override def equals(other: Any): Boolean =
      other match {
        case n: LeafNode =>
          this.sequenceID == n.sequenceID &&
            this.from == n.from
        case _ => false
      }

    override def hashCode: Int =
      31 * (31 * (31 + this.sequenceID) + this.from) + this.n

  }

  object LeafNode {
    /**
     * Extractor for a leaf node (parent, sequence ID, from, n, suffix link)
     *
     */
    def unapply(leaf: LeafNode): Option[(Node, SequenceID, Int, Int, Option[Node])] =
      Some(leaf.parent, leaf.sequenceID, leaf.from, leaf.n, leaf.suffix_link)

  }

  final class Root(
      val childrenMap: Nodes = InnerTree.emptyNodes()) extends Node {
    def size(): Int =
      1 + children.map { _.size() }.sum

    def parents() = List()

    def add[Alphabet](child: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit = {
      val value = start(child).merge // important line to obtain the actual value of the first item of the edge-label
      childrenMap(value) = child
    }

    def remove[Alphabet](node: NonRootNode, start: NonRootNode => Either[Alphabet, TerminalSymbol]): Unit = {
      val value = start(node).merge // important line to obtain the actual value of the first item of the edge-label
      this.childrenMap.remove(value)
    }

    /**
     * Adds a leaf to the root given a leaf ID and the sequence ID to which the leaf is referring.
     *
     * This leaf is given number 1.
     */
    def addLeaf[Alphabet](seqID: SequenceID, start: NonRootNode => Either[Alphabet, TerminalSymbol]): LeafNode = {
      val leaf = new LeafNode(
        parent = this,
        sequenceID = seqID,
        from = 1,
        n = 1)
      this.add(leaf, start)
      leaf
    }

    def pathLabel: List[NonRootNode] = List()
    def pathSingleLabel(getSequenceLengthBy: SequenceID => Int) = List()

    def walkDownUntilUnmatch[Alphabet](
      s: Walkable[Either[Alphabet, TerminalSymbol]],
      startIndex: Int,
      endIndex: Int)(
        start: NonRootNode => Either[Alphabet, TerminalSymbol],
        getEdgeLabelItem: (NonRootNode, Int) => Either[Alphabet, TerminalSymbol],
        labelLength: NonRootNode => Int): Option[(NonRootNode, Int, Int)] = {

      debugging {
        println(s"Starting to walk down until the sequence is reached: $s")
      }

      val first = s(startIndex)

      this.childStartingWith(first) match {
        case Some(nonRootNode) =>
          debugging {
            println(s"Go down to node: $nonRootNode")
          }
          nonRootNode.walkDownUntilUnmatch[Alphabet](s, startIndex, endIndex)(start, getEdgeLabelItem, labelLength)
        case None =>
          None
      }

    }

    def mkString[Alphabet](label: NonRootNode => String): String =
      (for {
        child <- children
      } yield child.mkString(label, "")).mkString("\n")

    override def toString = s"Root(nChildren=${children.size})"

    override def equals(other: Any): Boolean =
      other match {
        case r: Root =>
          this eq r // reference equality

        case _ => false
      }

    override def hashCode: Int =
      31 + this.children.size
  }

  object Root {
    /**
     * Builds an empty root
     */
    def apply(): Root =
      new Root()

    def unapply(root: Root): Option[Nodes] =
      Some(root.childrenMap)
  }
}