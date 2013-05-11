package math

/**
 * Relation
 *
 * A relation under some representation in T
 *
 */

trait Relation[T, A <: Set[T], B <: Set[T]] {

  def hold(a: A, b: B): Boolean

  def conjunct(another: Relation[T, A, B]): Relation[T, A, B]
  def disjunct(another: Relation[T, A, B]): Relation[T, A, B]
  def complement(): Relation[T, A, B]

  def converse(): Relation[T, A, B]

  def composite[C <: Set[T]](another: Relation[T, B, C]): Relation[T, A, C]
}

object Relation {
  val empty = ???
  val complete = ???
  val id = ???
}