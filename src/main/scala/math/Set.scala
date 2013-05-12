package math

import logic.Forall
import logic.Forsome

/**
 * Set
 *
 * A set under some representation in T, and also T is treated as the
 * universal set
 *
 */

trait Set[T] {
  
  val forall: Forall[T] = ???
  val forsome: Forsome[T] = ???

  def contains(entry: T): Boolean
  def includes(another: Set[T]): Boolean

  def union(another: Set[T]): Set[T]
  def intersect(another: Set[T]): Set[T]
  def disjoint(another: Set[T]): Set[T]

  def complement(): Set[T]

}

case class DefinableSet[T](val definition: T => Boolean) extends Set[T] {
  
  def contains(entry: T): Boolean = definition(entry)
  def includes(another: Set[T]): Boolean = {
    another.forall((anotherElem) => definition(anotherElem))
  }

  def union(another: Set[T]): Set[T] = {
    another match {
      case DefinableSet(anotherDefinition) => new DefinableSet({ x => this.definition(x) || anotherDefinition(x) })
      case _ => ???
    }
  }

  def intersect(another: Set[T]): Set[T] = {
    another match {
      case DefinableSet(anotherDefinition) => new DefinableSet({ x => this.definition(x) && anotherDefinition(x) })
      case _ => ???
    }
  }

  def disjoint(another: Set[T]): Set[T] = {
    another match {
      case DefinableSet(anotherDefinition) => new DefinableSet({ x => this.definition(x) && !anotherDefinition(x) })
      case _ => ???
    }
  }

  def complement(): Set[T] = new DefinableSet({ x => !this.definition(x) })

}

class Empty[T] extends DefinableSet[T]({ _ => false }) {
  object Qualification {
    val forall : Forall[T] = new Forall[T] {
      def apply(predicate: T => Boolean): Boolean = true
    }
    val forsome : Forsome[T] = new Forsome[T] {
      def apply(predicate: T => Boolean): Boolean = false
    }
  }
}

object Set {
  val emptySet: Empty[Any] = new Empty[Any]()
}

