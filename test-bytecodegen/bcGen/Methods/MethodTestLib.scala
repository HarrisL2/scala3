package bcGen.Methods

object MethodTestLib {

  def identity[T](value: T): T = value
  
  def first[U, V](fst: U, snd: V): U = fst
}
