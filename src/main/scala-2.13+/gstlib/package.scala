import scala.collection.immutable.ArraySeq
import scala.collection.mutable

package object gstlib {
  type Factory[Alphabet, Repr] = scala.collection.Factory[Alphabet, Repr]

  trait Builder[-A, +To] extends mutable.Builder[A, To]


  // method copyArrayToImmutableIndexedSeq in class LowPriorityImplicits2 is deprecated (since 2.13.0):
  // Implicit conversions from Array to immutable.IndexedSeq are implemented by copying;
  // Use the more efficient non-copying ArraySeq.unsafeWrapArray or an explicit toIndexedSeq cal
  def unsafeArrayToSeq[T](a: Array[T]): Seq[T] = ArraySeq.unsafeWrapArray(a)


}
