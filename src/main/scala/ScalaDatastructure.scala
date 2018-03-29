import datastructure.immutable.tree.{BinarySearchTree, BinaryTree}


object ScalaDatastructure {

  def main(args: Array[String]): Unit = {

    val tree = BinaryTree(10, 3, 6, 15, 45)
    val tree2 = BinaryTree(10, 3, 6, 15, 45).flip

    println(tree.flipEqual(tree2))


    implicit def empToOrderedEmp(emp: Emp): Ordered[Emp] = emp2 => emp.age.compareTo(emp2.age)
    val bst = BinarySearchTree(Vector(Emp("Amr", 8), Emp("Ahmed", 5), Emp("Hossam", 12)))

    val bst1 = BinarySearchTree(Vector(8, 3, 12))
    println(bst1.preOrderTraversal)

    println(bst1.find(3))
    val mappedBst1 = bst1.map(x => x * 2)
    println(mappedBst1.preOrderTraversal)

  }

  def sum(a: Int, b: Int)(c: Int, d: Int) : Int = {
    a + b + c + d
  }

}

case class Emp(name: String, age: Int)



