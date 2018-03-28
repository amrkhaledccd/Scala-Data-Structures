package datastructure.immutable.tree

case object LeafNode extends BinarySearchTree[Nothing]

case class BranchNode[+A <% Ordered[A]](data: A, left: BinarySearchTree[A], right: BinarySearchTree[A]) extends BinarySearchTree[A]

object BinarySearchTree {

  /*
   * Constructs Binary search tree from Vector
   * Sort the array
   * Make the middle of the array the root
   * Recursively do same for left half and right half
   * val tree = BinarySearchTree(Vector(10, 12, 6, 14))
   */
  def apply[A <% Ordered[A]](items: Vector[A]): BinarySearchTree[A] = {
    if(items.isEmpty) {
      LeafNode
    }
    else{

      def bst(items: Vector[A]): BinarySearchTree[A] = items.size match {
          case 0 => LeafNode
          case _ => {
            val half = items.size / 2
            BranchNode(items(half), bst(items.take(half)), bst(items.drop(half + 1)))
          }
        }

      bst(items.sorted)
    }
  }
}

class BinarySearchTree[+A <% Ordered[A]] {

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
  def compare[A](that: BinarySearchTree[A]): Boolean = (this, that) match {
    case (LeafNode, LeafNode) => true
    case (BranchNode(d1, l1, r1), BranchNode(d2, l2, r2)) if (d1 == d2) => l1.compare(l2) && r1.compare(r2)
    case _ => false
  }

  /*
    Adds new node
   */
  def insert[B >: A <% Ordered[B]](data: B): BinarySearchTree[B] = this match {
    case LeafNode => BranchNode(data, LeafNode, LeafNode)
    case BranchNode(d, l, r) => if(data < d) BranchNode(d,l.insert(data), r) else BranchNode(d,l, r.insert(data))
  }

  /*
    Visits node -> left -> right
   */
  def preOrderTraversal: List[A] =  {

    def loop(tree: BinarySearchTree[A], accumulator: List[A]): List[A] = tree match {
      case LeafNode => accumulator
      case BranchNode(d, l, r) => d :: loop(l, loop(r, accumulator))
    }

    loop(this, Nil)
  }

  /*
    Visits left -> node -> right
   */
  def inOrderTraversal: List[A] = {

    def loop(tree: BinarySearchTree[A], accumulator: List[A]) : List[A]= tree match {
      case LeafNode => accumulator
      case BranchNode(d, l, r) => loop(l, d :: loop(r, accumulator))
    }

    loop(this, Nil)
  }

  /*
    Visits left -> right -> node
   */
  def postOrderTraversal: List[A] = {

    def loop(tree: BinarySearchTree[A], accumulator: List[A]): List[A] = tree match {
      case LeafNode => Nil
      case BranchNode(d, l, r) => loop(l, loop(r, d :: accumulator))
    }

    loop(this, Nil)
  }
}
