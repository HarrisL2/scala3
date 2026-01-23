
trait Trait[W]

class A[U](u : U)

class B[T](t: T) extends A[T](t: T) with Trait[T]

trait T2{
    type X
}

// rm -tf library/target

class B1[U] extends T2{type X = U}