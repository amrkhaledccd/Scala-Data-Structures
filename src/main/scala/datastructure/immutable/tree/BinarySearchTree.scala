package datastructure.immutable.tree

case object LeafNode extends BinarySearchTree[Nothing]

case class BranchNode[+A](value: A, left: BinarySearchTree[A], right: BinarySearchTree[A]) extends BinarySearchTree[A]

object BinarySearchTree {

  /*
   * Initializes Binary search tree
   * val tree = BinarySearchTree(10, 12, 6, 14)
   */
  def apply[A](items: A*): BinarySearchTree[A] = {
    if(items.isEmpty) {
      LeafNode
    }
    else{
      BranchNode(items.head, apply(items.tail.take(items.size/2): _*), apply(items.tail.drop(items.size/2): _*))
    }
  }
}

class BinarySearchTree[+A] {

  /*
      Return the size of the tree
      Size of the tree is the number of nodes in the tree (Leaf nodes excluded)
    */
  def size: Int = this match {
    case LeafNode => 0
    case BranchNode(_, l, r) => 1 + l.size + r.size
  }

  /*
    Depth is the length of the longest path from root to leaf
   */
  def depth: Int = this match {
    case LeafNode => 0
    case BranchNode(_, l, r) => 1 + l.depth max r.depth
  }

  /*
    Compares two trees
    return true if they are equal (values and structure)
   */
  def compare[B >: A](that: BinarySearchTree[B]): Boolean = (this, that) match {
    case (LeafNode, LeafNode) => true
    case (BranchNode(v1, l1, r1), BranchNode(v2, l2, r2)) if (v1 == v2) => l1.compare(l2) && r1.compare(r2)
    case _ => false
  }

}
