package math

/** Function
  *
  * A function under some representation in S and T
  *
  */

trait Function[S, T] {

  def domian() : Set[S]
  def codomain() : Set[T]

  def lid() : Function[S, S]
  def rid() : Function[T, T]

  def apply( x : S ) : T

  def composite[U]( another : Function[T, U] ) : Function[S, U]

}

case class DefinableFunction[S, T]( val dom : Set[S], val codom : Set[T], val definition : S => T )
    extends Function[S, T] {

  def domian() : Set[S] = dom
  def codomain() : Set[T] = codom

  def lid() : Function[S, S] = {
    new DefinableFunction[S, S]( dom, dom, ( x ) => x )
  }
  def rid() : Function[T, T] = {
    new DefinableFunction[T, T]( codom, codom, ( x ) => x )
  }

  def apply( x : S ) : T = definition( x )

  def composite[U]( another : Function[T, U] ) : Function[S, U] = another match {
    case DefinableFunction( anotherDom, anotherCodom, anotherDefinition ) => {
      new DefinableFunction( dom, anotherCodom, ( x ) => anotherDefinition( definition( x ) ) )
    }
    case _ => ???
  }

}
