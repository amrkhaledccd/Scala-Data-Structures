
import datastructure.immutable.list.{LinkedList, Stack}
import datastructure.immutable.tree.BinaryTree



object ScalaDatastructure {

  def main(args: Array[String]): Unit = {

//    val fun = sum(1,2)_
//
//    val result = fun(3, 4)
//
//    println(result)


//    val stack = Stack(1, 2, 4) //++ Stack(5, 6, 7)
//    println(stack)

    //println(stack.tail)


 //   val test = new MyTest[ChildOne]


//    val childOnes = List[ChildOne](new ChildOne)
//
//    val childTwo: List[Parent] = new ChildTwo :: childOnes

//    val l = LinkedList(1, 2, 3, 4)
//    val n = l ++ LinkedList(6, 7)
//
//    val  m = n.append(102)

    //println(l.reverse)
    //println(m)

    val tree = BinaryTree(10, 3, 6, 15, 45)

    val tree2 = BinaryTree(10, 3, 6, 15, 45)

    println(tree.flipEqual(tree2))
  }

  def sum(a: Int, b: Int)(c: Int, d: Int) : Int = {
    a + b + c + d
  }

}


class MyTest[T <: ChildOne]


class Parent

class ChildOne extends Parent

class ChildTwo extends Parent



