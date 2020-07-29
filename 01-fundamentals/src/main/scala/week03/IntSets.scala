package week03

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty(root: Int, left: IntSet, rigth: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < root) left contains x
    else if (x > root) rigth contains x
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < root) new NonEmpty(root, left incl x, rigth)
    else if (x > root) new NonEmpty(root, left, rigth incl x)
    else this
  }
  def union(other: IntSet): IntSet =
    ((left union rigth) union other) incl root

  override def toString = "{" + left + root + rigth + "}"
}
