import scala.collection.immutable.ArraySeq
import scala.collection.mutable

package object gstlib {
  type Factory[Alphabet, Repr] = scala.collection.Factory[Alphabet, Repr]

  trait Builder[-A, +To] extends mutable.Builder[A, To]

}
