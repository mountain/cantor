package math

import scala.collection.Traversable

/**
 * Set
 *
 * A set under some representation in T, and also T is treated as the
 * universal set
 *
 */

trait Set[T] {

  def contains(entry: T): Boolean
  //def includes(another: Set[T]): Boolean

  def union(another: Set[T]): Set[T]
  def intersect(another: Set[T]): Set[T]
  def disjoint(another: Set[T]): Set[T]

  def complement(): Set[T]

}

trait Enumerable[T] extends Set[T] with Traversable[T] {

}

case class Definable[T](val definition: T => Boolean) extends Set[T] {

  def contains(entry: T): Boolean = definition(entry)

  def union(another: Set[T]): Set[T] = {
    another match {
      case Definable(anotherDefinition) => new Definable({ x => this.definition(x) || anotherDefinition(x) })
      case _ => ???
    }
  }

  def intersect(another: Set[T]): Set[T] = {
    another match {
      case Definable(anotherDefinition) => new Definable({ x => this.definition(x) && anotherDefinition(x) })
      case _ => ???
    }
  }

  def disjoint(another: Set[T]): Set[T] = {
    another match {
      case Definable(anotherDefinition) => new Definable({ x => this.definition(x) && !anotherDefinition(x) })
      case _ => ???
    }
  }

  def complement(): Set[T] = new Definable({ x => !this.definition(x) })

}

class Empty[T] extends Definable[T]({ _ => false }: T => Boolean) {
}

object Set {

  val empty: Empty[Any] = new Empty[Any]()

}

