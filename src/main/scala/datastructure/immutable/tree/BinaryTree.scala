package datastructure.immutable.tree


case object Leaf extends BinaryTree[Nothing]

case class Branch[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {

  def apply[A](items: A*): BinaryTree[A] = {
    if(items.isEmpty) {
      Leaf
    }
    else{
      Branch(items.head, apply(items.tail.take(items.size/2): _*), apply(items.tail.drop(items.size/2): _*))
    }
  }
}

class BinaryTree[+A] {

  def size: Int = this match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + l.size + r.size
  }

  def depth: Int = this match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + l.depth max r.depth
  }

  def compare[B >: A](that: BinaryTree[B]): Boolean = (this, that) match {
    case (Leaf, Leaf) => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if (v1 == v2) => l1.compare(l2) && r1.compare(r2)
    case _ => false
  }

  def flip: BinaryTree[A] = this match {
    case Leaf => Leaf
    case Branch(v, l, r) => Branch(v, r.flip, l.flip)
  }

  def flipEqual[B >: A](that: BinaryTree[B]): Boolean = (this, that) match {
    case (Leaf, Leaf) => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if(v1 == v2) => l1.flipEqual(r2) && r1.flipEqual(l2)
    case _ => false
  }

}
