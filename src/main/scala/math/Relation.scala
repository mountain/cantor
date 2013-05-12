package math

/**
 * Relation
 *
 * A relation under some representation in T
 *
 */

trait Relation[T, A <: Set[T], B <: Set[T]] {
  
  def domian(): A
  def codomain(): B

  def holds(a: T, b: T): Boolean
  
  def includes(another: Relation[T, A, B]): Boolean

  def conjunct(another: Relation[T, A, B]): Relation[T, A, B]
  def disjunct(another: Relation[T, A, B]): Relation[T, A, B]
  def complement(): Relation[T, A, B]

  def inverse(): Relation[T, B, A]

  def composite[C <: Set[T]](another: Relation[T, B, C]): Relation[T, A, C]

}

case class DefinableRelation[T, A <: Set[T], B <: Set[T]](val dom: A, val codom: B, val definition: (T, T) => Boolean)
  extends Relation[T, A, B] {

  def domian(): A = dom
  def codomain(): B = codom
 
  def holds(a: T, b: T): Boolean = definition(a, b)
  
  def includes(another: Relation[T, A, B]): Boolean = 
    another match {
      case DefinableRelation(anotherDom, anotherCodom, anotherDefinition) =>
      anotherCodom.forall((anotherTarget) => anotherDom.forall((anotherSource) => definition(anotherSource, anotherTarget)))
      case _ => ???
    }

  def conjunct(another: Relation[T, A, B]): Relation[T, A, B] = {
    another match {
      case DefinableRelation(anotherDom, anotherCodom, anotherDefinition) =>
        new DefinableRelation(dom, codom, (x, y) => this.definition(x, y) || anotherDefinition(x, y))
      case _ => ???
    }
  }

  def disjunct(another: Relation[T, A, B]): Relation[T, A, B] = {
    another match {
      case DefinableRelation(anotherDom, anotherCodom, anotherDefinition) =>
        new DefinableRelation(dom, codom, (x, y) => this.definition(x, y) && anotherDefinition(x, y))
      case _ => ???
    }
  }

  def complement(): Relation[T, A, B] = {
    new DefinableRelation(dom, codom, (x, y) => !this.definition(x, y))
  }

  def inverse(): Relation[T, B, A] = {
    new DefinableRelation(codom, dom, (x, y) => this.definition(y, x))
  }

  def composite[C <: Set[T]](another: Relation[T, B, C]): Relation[T, A, C] = {
    another match {
      case DefinableRelation(anotherDom, anotherCodom, anotherDefinition) =>
        new DefinableRelation(dom, anotherCodom, (u, w) => codom.forsome(v => this.definition(u, v) && anotherDefinition(v, w)))
      case _ => ???
    }
  }
}

object Relation {
  val empty = ???
  val complete = ???
  val id = ???
}