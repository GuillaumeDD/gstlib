# gstlib: Generalized Suffix Tree Library in Scala #

gstlib  is  a   library  that  implements  a   generalized  suffix  tree
datastructure for sequences of items in the Scala programming language.

Latest version: 0.1

Features:

- efficient building of suffix trees and generalized suffix trees;
- efficient search of a pattern in (generalized) suffix trees;
- efficient computation of longest common subsequence of two sequences;
- linear-time solution to the multiple common substring problem.

It  implements the  following  algorithms from  the  book "Algorithm  on
Strings,  Trees,  and  Sequences:  Computer  Science  and  Computational
Biology" by D. Gusfield:

- Ukkonen's algorithm for building generalized suffix tree;
- constant-time  lowest common  ancestor (LCA)  retrieval (Schieber  and
  Vishkin approach) via a linear-time preprocessing of the tree;
- a linear-time solution to the multiple common substring problem (Lucas
  Hui approach).

## Getting gstlib ##

gstlib is available for Scala 2.11 and 2.12.

Usage with SBT, adding a dependency  to the latest version of the gstlib
to your sbt build definition file:

```scala
libraryDependencies += "com.github.guillaumedd" %% "gstlib" % "0.1"
```

## Usage ##

### Suffix Tree ###

```scala
import gstlib.SuffixTree

/*
 * Building the suffix tree
 */
val str = "String to be searched"
val stree = SuffixTree(str)

/*
 * Searching for a pattern
 */
val pattern = "to be"
stree.contains(pattern) // res0: Boolean = true

val indexes = stree.find(pattern) // indexes: List[Int] = List(7)
val first = indexes.head // indexes: List[Int] = List(7)

stree.sequence.substring(first, first + pattern.length)
// res1: String = to be

/*
 * Traversing the suffixes
 */
for (suffix <- stree.suffixes()) {
  println(suffix)
}

/*
 * It is possible to build suffix tree for a wide variety of sequences!
 */
val sentence = str.split(" ") // sentence: Array[String] = Array(String, to, be, searched)
val streeSentence = SuffixTree(sentence)

val patternSentence = Array("be", "searched")

streeSentence.contains(patternSentence) // res3: Boolean = true

val indexesSentence = streeSentence.find(patternSentence) // indexesSentence: List[Int] = List(2)
val firstSentence = indexesSentence.head // firstSentence: Int = 2

streeSentence.sequence.slice(firstSentence, firstSentence + patternSentence.length)
// res4: Array[String] = Array(be, searched)

streeSentence.contains(Array("hello", "world", "!")) // res5: Boolean = false
```

### Generalized Suffix Tree ###

```scala
import gstlib.GeneralizedSuffixTree

/*
 * Building a generalized suffix tree
 *
 * Here:
 * - a sequence is an array of string, and
 * - an item is a string
 */
val utterances = List(
  Array("Hello", "world", "!"),
  Array("How", "are", "you", "today", "?"),
  Array("How", "are", "you", "doing", "?"),
  Array("are", "you", "there", "?"),
  Array("What", "a", "beautiful", "world", "!")
)

val gstree = GeneralizedSuffixTree(utterances: _*)

/*
 * Searching the generalized suffix tree
 */
val pattern = Array("are", "you")
gstree.find(pattern)
// res0: List[(gstlib.GeneralizedSuffixTreeBuilder.SequenceID, Int)] = List((2,1), (1,1), (3,0))
// (2,1) in "How", "are", "you", "doing", "?"
// (1,1) in "How", "are", "you", "today", "?"
// (3,0) in "are", "you", "there", "?"

/*
 * Computing the multiple common subsequence
 */
for ((freq, subseq) <- gstree.bulkMultipleCommonSubsequence()) {
  val subseqStr = subseq.mkString(" ")
  println(s"The sequence '$subseqStr' is appearing in $freq sequences")
}
// Output:
// The sequence 'are you' is appearing in 3 sequences
// The sequence '!' is appearing in 2 sequences
// The sequence '?' is appearing in 3 sequences
// The sequence 'How are you' is appearing in 2 sequences
// The sequence 'you' is appearing in 3 sequences
// The sequence 'world !' is appearing in 2 sequences

/*
 * Computing the longest common subsequences between a pattern and a
 * generalized suffix tree
 */
val pattern2 = Array("well", ",", "today", "?", "where", "are", "you", "?")
for {
  (start, end, positions) <- gstree.findLongestCommonSubsequences(pattern2)
} {
  val subsequence = pattern2.slice(start, end).mkString(" ")
  println(s"Subsequence '$subsequence' appears in:")
  for {
    (seqID, startPos) <- positions
  } {
    println(s"\t- sequence $seqID: ${gstree.getSequenceBy(seqID).mkString(" ")} at starting position $startPos")
  }
}
// Output:
//Subsequence 'today ?' appears in:
//	- sequence 1: How are you today ? at starting position 3
//Subsequence 'are you' appears in:
//	- sequence 2: How are you doing ? at starting position 1
//	- sequence 1: How are you today ? at starting position 1
//	- sequence 3: are you there ? at starting position 0

/*
 * It is possible to build generalized suffix tree for a wide variety
 * of sequence!
 *
 * Here:
 * - a sequence is a string, and
 * - an item is a character
 */
val gstree2 = GeneralizedSuffixTree("ABCDEF", "CDE", "EFGHIJK", "LMNOPQRST", "STUVWXYZ")
gstree2.contains("E") // res2: Boolean = true
gstree2.find("E") // res3: List[(gstlib.GeneralizedSuffixTreeBuilder.SequenceID, Int)] = List((2,0), (0,4), (1,2))
// (2,0) in EFGHIJK
// (0,4) in ABCDEF
// (1,2) in CDE

/*
 * Building a generalized suffix tree via a Builder
 */
val builder = GeneralizedSuffixTree.newBuilder[Char, String]
builder += "foo"
builder += "bar"

val gstreeFooBar = builder.result()

gstreeFooBar.contains("hello") // res7: Boolean = false
gstreeFooBar.contains("oo") // res8: Boolean = true
gstreeFooBar.contains("ba") // res9: Boolean = true
```

### Utils: Longest Common Subsequence ###

```scala
import gstlib.GeneralizedSuffixTree.longestSubsequence

longestSubsequence("abcd", "efgh") // res0: Option[String] = None

longestSubsequence("abcd", "cdefgh") // res1: Option[String] = Some(cd)
```

## Benchmarks ##

This  project  includes benchmarks  to  assess  the efficiency  of  some
algorithm (building of tree, solving  of the multiple common subsequence
problem). Benchmarks can be executed via SBT:

	$ sbt test:run

Files to benchmark the code come from the repository:
[schmidda/ukkonen-suffixtree](https://github.com/schmidda/ukkonen-suffixtree).

## Contributors ##

- Guillaume Dubuisson Duplessis (2016-2017)
- Vincent Letard (2016)

## License ##

CECILL-B - see the LICENSE file.
