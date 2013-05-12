package math

/** Relation
  *
  * A relation under some representation in T
  *
  */

trait Relation[T] {

  def domian() : Set[T]

  def empty() : Relation[T]
  def complete() : Relation[T]
  def id() : Relation[T]

  def holds( a : T, b : T ) : Boolean

  def includes( another : Relation[T] ) : Boolean

  def conjunct( another : Relation[T] ) : Relation[T]
  def disjunct( another : Relation[T] ) : Relation[T]
  def complement() : Relation[T]

  def inverse() : Relation[T]

  def composite( another : Relation[T] ) : Relation[T]

  def functional() : Boolean = {
    this.id includes ( this.inverse.composite( this ) )
  }

  def total() : Boolean = {
    ( this.inverse composite ( this ) ) includes ( this.id )
  }

  def injective() : Boolean = {
    this.id includes ( this composite this.inverse )
  }

  def surjective() : Boolean = {
    (this composite this.inverse ) includes  this.id
  }

  def reflexive() : Boolean = {
    this includes  this.id
  }

  def irreflexive() : Boolean = {
    this.empty includes ( this conjunct this.id )
  }

  def transitive() : Boolean = {
    this includes ( this composite this )
  }

  def symmetric() : Boolean = {
    ( this includes this.inverse ) && ( this.inverse includes this )
  }

  def antisymmetric() : Boolean = {
    this.id includes ( this conjunct this.inverse )
  }

}

case class DefinableRelation[T]( val dom : Set[T], val definition : ( T, T ) => Boolean )
    extends Relation[T] {

  def domian() : Set[T] = dom

  def holds( a : T, b : T ) : Boolean = definition( a, b )

  def includes( another : Relation[T] ) : Boolean =
    another match {
      case DefinableRelation( anotherDom, anotherDefinition ) =>
        anotherDom.forall( ( anotherTarget ) => anotherDom.forall( ( anotherSource ) => definition( anotherSource, anotherTarget ) ) )
      case _ => ???
    }

  def conjunct( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherDefinition ) =>
        new DefinableRelation( dom, ( x, y ) => this.definition( x, y ) && anotherDefinition( x, y ) )
      case _ => ???
    }
  }

  def disjunct( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherDefinition ) =>
        new DefinableRelation( dom, ( x, y ) => this.definition( x, y ) || anotherDefinition( x, y ) )
      case _ => ???
    }
  }

  def complement() : Relation[T] = {
    new DefinableRelation( dom, ( x, y ) => !this.definition( x, y ) )
  }

  def inverse() : Relation[T] = {
    new DefinableRelation( dom, ( x, y ) => this.definition( y, x ) )
  }

  def composite( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherDefinition ) =>
        new DefinableRelation( dom, ( u, w ) => dom.forsome( v => this.definition( u, v ) && anotherDefinition( v, w ) ) )
      case _ => ???
    }
  }

  def empty() : Relation[T] = {
    new DefinableRelation( dom, ( x, y ) => false )
  }

  def complete() : Relation[T] = {
    new DefinableRelation( dom, ( x, y ) => true )
  }

  def id() : Relation[T] = {
    new DefinableRelation( dom, ( x, y ) => x == y )
  }

}