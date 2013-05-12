package math

/** Function
  *
  * A function under some representation in T
  *
  */

trait Function[S, T] {

  def domian() : Set[S]
  def codomain() : Set[T]

  def lid() : Function[S, S]
  def rid() : Function[T, T]

  def apply( a : S ) : T

  def composite[U]( another : Function[T, U] ) : Function[S, U]

}