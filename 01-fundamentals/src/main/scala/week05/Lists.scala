package object week05 {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("Empty List")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => concat(reverse(ys), List(y))
  }

  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ++ (xs drop (n + 1))
}
