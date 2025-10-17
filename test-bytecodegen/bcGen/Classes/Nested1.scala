package bcGen.Classes

class Nested1[T](val initOuterT: T) {
    val outer1T: T = initOuterT
    class Inner1[U](val initInner1U: U, val initInner1T: T) {
        val inner1U: U = initInner1U
        val inner1T: T = initInner1T
        class Inner2_0[V](val initInner2V: V) extends Pair[T, U](outer1T, inner1U) {
            val inner2_0V : V = initInner2V
            def foo(x: T, y: U, z: V): String = s"Outer1: $x, Inner1: $y, Inner2: $z"
            def bar(): String = s"Outer1: $outer1T, Inner1: $inner1U, Inner2: $inner2_0V"
        }
        class Inner2_1[V] {
            def foo(x: T, y: U, z: V): String = s"Outer1: $x, Inner1: $y, Inner2: $z"
        }
    }
    class Inner1_1[U] {
        class Inner2_0[V] {
            def foo(x: T, y: U, z: V): String = s"Outer1: $x, Inner1_1: $y, Inner2: $z"
        }
        class Inner2_1[V] {
            def foo(x: T, y: U, z: V): String = s"Outer1: $x, Inner1_1: $y, Inner2: $z"
        }
    }
}

object testNested {
    val intOuter1 = new Nested1[Int](42)
    val refInner1 = new intOuter1.Inner1[String]("Hello", intOuter1.outer1T)
    val doubleInner2_0 = new refInner1.Inner2_0[Double](3.14)
    @main def testNested1(): Unit = {
        println(doubleInner2_0.foo(41, "World", 2.71))
    }

    @main def testNested2(): Unit = {
        println(doubleInner2_0.bar())
    }
}