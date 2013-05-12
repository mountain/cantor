package logic

trait Forall[U] {

  def apply(predicate: U => Boolean): Boolean

}
