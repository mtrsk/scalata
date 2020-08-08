package week05

package object merge {
  import math.Ordering

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xxs, y :: yys) =>
        if (ord.lt(x, y)) x :: merge(xxs, ys)
        else y :: merge(xs, yys)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}
