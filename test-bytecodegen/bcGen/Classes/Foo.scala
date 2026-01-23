class Outer[T] {
  def a: Int = ???
  trait Inner[U] {
    val tfield: T = ???
    val ufield: U = ???
    def b = a
  }
}

class Foo {
  val outer = new Outer[Int]
  class C extends outer.Inner[String]
}

/*

*/