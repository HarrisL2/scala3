package bcGen.Traits

// trait Id[U] {
//     var v: U
//     def get: U = v
//     def set(x: U): Unit = { v = x }
//     def id(x: U): U = x
// }

// class IdImpl[T](initial: T) extends Id[T] {
//     var v: T = initial
// }

// class IdIntImpl(initial: Int) extends Id[Int] {
//     var v: Int = initial
// }
// trait Derived[T] extends Id[T] {
//     def twice(x: T): (T, T) = (id(x), id(x))
// }

trait TpA[A](fieldA: A) { def a(a: A): A = a}
trait TpB[B] { def b(b: B): B = b}

class AandB(x: Int) extends TpA[Int](x) with TpB[Double] {
    override def a(a: Int): Int = a
    override def b(b: Double): Double = b
}

class TandU[T, U](fieldT: T) extends TpA[T](fieldT: T) with TpB[U] {
}

// object testTrait {
//     @main def testIdInt(): Unit = {
//         val idInt = new IdImpl[Int](42)
//         println(s"Initial value: ${idInt.get}")
//     }

//     def testIdGeneric[T](i: Int, t: T): Unit = {
//          val idGen = new IdImpl[T](t)
//          println(s"Initial generic value: ${idGen.get}")
//     }
// }