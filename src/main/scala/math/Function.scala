package math

/**
 * Function
 *
 * A function under some representation in T
 *
 */

trait Function[T, A <: Set[T], B <: Set[T]] {

  def apply(a: A): B

}