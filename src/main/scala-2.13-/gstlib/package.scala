import scala.collection.generic.CanBuildFrom
import scala.collection.mutable


package object gstlib {

  implicit class CanBuildFromExt[Alphabet, Repr](cbf: CanBuildFrom[Repr, Alphabet, Repr]) {
    def newBuilder: mutable.Builder[Alphabet, Repr] = cbf()
  }

  type Factory[Alphabet, Repr] = CanBuildFrom[Repr, Alphabet, Repr]

  trait Builder[-A, +To] extends mutable.Builder[A, To] {
    def addOne(elem: A): this.type

    def +=(elem: A): this.type = {
      addOne(elem)
    }
  }

}
