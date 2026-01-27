package bcGen.Traits

trait Id[U, V] {
    var u: U
    var v: V
    def get: U = u
    def set(x: U): Unit = { u = x }
    def id(v: V): V = v
}

// get, set, id gets inlined in Mixin phase
// cannot directly copy the hints in ErasurePreservation since
// the hints are created in the context of the original method symbol

// also, hints are missing since ErasurePreservation runs before Mixin phase
// e.g. def get(): Object =
      // super[Id].get()
      // missing InvokeReturnType hint
class IdImpl[V, U](v1: V, u1: U) extends Id[U, V] {
    var u: U = u1
    var v: V = v1
}

class IdIntImpl(initial: Int) extends Id[Int, Int] {
    var u: Int = initial
    var v: Int = initial
}