package logic

trait Forall[T] {

  def apply(predicate: T => Boolean): Boolean

}
