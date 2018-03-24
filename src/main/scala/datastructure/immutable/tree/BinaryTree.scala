package datastructure.immutable.tree


case object Leaf extends BinaryTree[Nothing]

case class Branch[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {

  /*
   * Initializes Binary tree
   * val tree = BinaryTree(10, 12, 6, 14)
   */
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

  /*
      Return the size of the tree
      Size of the tree is the number of nodes in the tree (Leaf nodes excluded)
    */
  def size: Int = this match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + l.size + r.size
  }

  /*
    Depth is the length of the longest path from root to leaf
   */
  def depth: Int = this match {
    case Leaf => 0
    case Branch(_, l, r) => 1 + l.depth max r.depth
  }

  /*
    Compares two trees
    return true if they are equal (values and structure)
   */
  def compare[B >: A](that: BinaryTree[B]): Boolean = (this, that) match {
    case (Leaf, Leaf) => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if (v1 == v2) => l1.compare(l2) && r1.compare(r2)
    case _ => false
  }

  /*
    Flip the tree left and right nodes
   */
  def flip: BinaryTree[A] = this match {
    case Leaf => Leaf
    case Branch(v, l, r) => Branch(v, r.flip, l.flip)
  }

  /*
    Checks if "that" tree is the flip of the current tree (this)
   */
  def flipEqual[B >: A](that: BinaryTree[B]): Boolean = (this, that) match {
    case (Leaf, Leaf) => true
    case (Branch(v1, l1, r1), Branch(v2, l2, r2)) if(v1 == v2) => l1.flipEqual(r2) && r1.flipEqual(l2)
    case _ => false
  }

}
