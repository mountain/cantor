package math

/** Relation
  *
  * A relation under some representation in T
  *
  */

trait Relation[T] {

  def domian() : Set[T]
  def codomain() : Set[T]

  def empty() : Relation[T]
  def complete() : Relation[T]

  def id() : Relation[T]
  def lid() : Relation[T]
  def rid() : Relation[T]

  def holds( a : T, b : T ) : Boolean

  def includes( another : Relation[T] ) : Boolean

  def conjunct( another : Relation[T] ) : Relation[T]
  def disjunct( another : Relation[T] ) : Relation[T]
  def complement() : Relation[T]

  def inverse() : Relation[T]

  def composite[C <: Set[T]]( another : Relation[T] ) : Relation[T]

  def functional() : Boolean = {
    this.rid.includes( this.inverse.composite( this ) )
  }

  def total() : Boolean = {
    this.inverse.composite( this ).includes( this.rid )
  }

  def injective() : Boolean = {
    this.lid.includes( this.composite( this.inverse ) )
  }

  def surjective() : Boolean = {
    this.composite( this.inverse ).includes( this.lid )
  }

  def reflexive() : Boolean = {
    this.includes( this.id )
  }

  def transitive() : Boolean = {
    this.includes( this.composite( this ) )
  }

}

case class DefinableRelation[T, A <: Set[T], B <: Set[T]]( val dom : A, val codom : B, val definition : ( T, T ) => Boolean )
    extends Relation[T] {

  def domian() : A = dom
  def codomain() : B = codom

  def holds( a : T, b : T ) : Boolean = definition( a, b )

  def includes( another : Relation[T] ) : Boolean =
    another match {
      case DefinableRelation( anotherDom, anotherCodom, anotherDefinition ) =>
        anotherCodom.forall( ( anotherTarget ) => anotherDom.forall( ( anotherSource ) => definition( anotherSource, anotherTarget ) ) )
      case _ => ???
    }

  def conjunct( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherCodom, anotherDefinition ) =>
        new DefinableRelation( dom, codom, ( x, y ) => this.definition( x, y ) || anotherDefinition( x, y ) )
      case _ => ???
    }
  }

  def disjunct( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherCodom, anotherDefinition ) =>
        new DefinableRelation( dom, codom, ( x, y ) => this.definition( x, y ) && anotherDefinition( x, y ) )
      case _ => ???
    }
  }

  def complement() : Relation[T] = {
    new DefinableRelation( dom, codom, ( x, y ) => !this.definition( x, y ) )
  }

  def inverse() : Relation[T] = {
    new DefinableRelation( codom, dom, ( x, y ) => this.definition( y, x ) )
  }

  def composite[C <: Set[T]]( another : Relation[T] ) : Relation[T] = {
    another match {
      case DefinableRelation( anotherDom, anotherCodom, anotherDefinition ) =>
        new DefinableRelation( dom, anotherCodom, ( u, w ) => codom.forsome( v => this.definition( u, v ) && anotherDefinition( v, w ) ) )
      case _ => ???
    }
  }

  def empty() : Relation[T] = {
    new DefinableRelation( dom, codom, ( x, y ) => false )
  }

  def complete() : Relation[T] = {
    new DefinableRelation( dom, codom, ( x, y ) => true )
  }

  def id() : Relation[T] = {
    if ( !dom.empty().includes( dom.disjoint( codom ) ) )
      new DefinableRelation( dom, codom, ( x, y ) => x == y )
    else
      ???
  }

  def lid() : Relation[T] = {
    new DefinableRelation( dom, dom, ( x, y ) => x == y )
  }

  def rid() : Relation[T] = {
    new DefinableRelation( codom, codom, ( x, y ) => x == y )
  }

}