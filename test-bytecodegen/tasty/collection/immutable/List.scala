package tasty.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{tailrec, targetName}

abstract class List[+T] {
  def head: T
  def tail: List[T]
  def length: Int
  def isEmpty: Boolean = length == 0
  def contains[T1 >: T](elem: T1): Boolean

  def ::[T1 >: T](elem: T1): List[T1] = new ::(elem, this)

}

// @targetName("Cons")
case class ::[+T](head0: T, tail0: List[T]) extends List[T] {

  override def head: T = head0

  override def tail: List[T] = tail0

  override def length: Int = 1 + tail.length

  def contains[T1 >: T](elem: T1): Boolean = {
    var these: List[T] = this
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  @tailrec
  private final def tailHash(list: List[T], or: Int): Int = {
    val headHash = list.head.hashCode
    if (list.tail.isEmpty)
      headHash | or
    else
      tailHash(list.tail, or | headHash >> 8)
  }

  override def hashCode(): Int = {
    var these: List[T] = this
    var hashCode: Int = 0
    while (!these.isEmpty) {
      val headHash = these.head.hashCode()
      if (these.tail.isEmpty)
        hashCode = hashCode | headHash
      else
        hashCode = hashCode | headHash >> 8
      these = these.tail
    }
    hashCode
  }
}

case object Nil extends List[Nothing] {
  override def head: Nothing = throw new NoSuchElementException("head of empty list")

  override def tail: Nothing = throw new UnsupportedOperationException("tail of empty list")

  override def length: Int = 0

  override def contains[T1 >: Nothing](elem: T1): Boolean = false

  override def hashCode(): Int = 0
}