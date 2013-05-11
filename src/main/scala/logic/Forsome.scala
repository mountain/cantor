package logic

trait Forsome[T] {

  def apply(predicate: T => Boolean): Boolean

}