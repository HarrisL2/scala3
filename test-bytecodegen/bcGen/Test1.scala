class Box[T](value: T) {
    def get(): T = value
}

class Pair[K, V](val k: K, val v: V) {}

object Main {
    val b = Box[Double](1.2)
    val p = Pair[Int, Double](12, 3.4)
    def id[T](x: T): T = x

    def foo(): Unit = println(
        id[Int](42) + 
        b.get() + 
        p.v)
}

