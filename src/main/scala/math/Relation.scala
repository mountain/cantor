package math

/**
 * Relation
 *
 * A relation under some representation in T
 *
 */

trait Relation[T] {

  def hold(a: Set[T], b: Set[T]): Boolean

  def conjunct(another: Relation[T]): Relation[T]
  def disjunct(another: Relation[T]): Relation[T]
  def complement(): Relation[T]

  def converse(): Relation[T]

  def composite(another: Relation[T]): Relation[T]
}

object Relation {
  val empty = ???
  val complete = ???
  val id = ???
}