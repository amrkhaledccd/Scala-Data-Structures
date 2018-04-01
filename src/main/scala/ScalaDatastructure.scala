import datastructure.immutable.list.LinkedList
import datastructure.immutable.tree.{BinarySearchTree, BinaryTree}


object ScalaDatastructure {

  def main(args: Array[String]): Unit = {

//    val list = LinkedList(2, 4, 6, 8, 10)
//    list.traverse(x => print(s"$x "))
//    println()

    val linkedList = LinkedList(5, 6, 8, 1, 4, 15, 16)
    println(linkedList)

    println(linkedList.delete(2))

//    val prepended = LinkedList(1, 2, 3, 4, 5) ++ LinkedList(6, 7, 8, 9) append 10
//    println(prepended)
//
//    val tree = BinaryTree(10, 3, 6, 15, 45)
//    val tree2 = BinaryTree(10, 3, 6, 15, 45).flip
//
//    println(tree.flipEqual(tree2))
//
//
//    implicit def empToOrderedEmp(emp: Emp): Ordered[Emp] = that => emp.age.compareTo(that.age)
//    val bst = BinarySearchTree(Vector(Emp("Amr", 8), Emp("Ahmed", 5), Emp("Hossam", 12)))
//    println(bst.toInOrderList)
//
//    val bst1 = BinarySearchTree(Vector(8, 3, 12))
//    println(bst1.toPreOrderList)
//
//    println(bst1.find(3))
//
//    val mappedBst1 = bst1.map(x => x * 2)
//    println(mappedBst1.toPreOrderList)
//
//    bst1.postOrderTraversal(x => print(s"$x "))

  }

  def sum(a: Int, b: Int)(c: Int, d: Int) : Int = {
    a + b + c + d
  }

}

case class Emp(name: String, age: Int)




