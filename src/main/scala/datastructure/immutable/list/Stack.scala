package datastructure.immutable.list

/*
   This is a naive implementation
   It is better if I used tail recursion
   look at LinkedList for tail recursion example
* */
trait Stack[+T] {
  def isEmpty: Boolean

  def push[U >: T](t: U): Stack[U] = NonEmpty(t, this)

  def pop: (T, Stack[T])

  def ++[U >: T](stack: Stack[U]): Stack[U]

  def update[U >: T](t: U, i: Int): Stack[U]

  def head: T

  def tail: Stack[T]
}

case object Empty extends Stack[Nothing] {
  def isEmpty = true

  def ++[U](stack: Stack[U]): Stack[U]  = NonEmpty(stack.head, stack.tail)

  def head: Nothing = throw new NoSuchElementException

  def tail: Nothing = throw new NoSuchElementException

  def pop: Nothing = throw new NoSuchElementException

  def update[U](t: U, i: Int): Stack[U] = throw new IndexOutOfBoundsException
}

case class NonEmpty[+T](hd: T, tl: Stack[T]) extends Stack[T] {

  def isEmpty: Boolean = false

  //remove and return the head of the stack
  def pop: (T, Stack[T]) = (hd, tl)

  //Appends stack in front of the current stack
  def ++[U >: T](stack: Stack[U]): Stack[U] = NonEmpty(head, tail ++ stack)

  //add an item at index
  def update[U >: T](t: U, i: Int): Stack[U] = if(i == 0) NonEmpty(t, this) else NonEmpty(head,tail.update(t, i-1))

  //Return head of the stack
  def head: T = hd

  // Return all stack elements without head
  def tail: Stack[T] = tl
}

object Stack {

  /*
     initializes the stack
     val stack = Stack(1, 2, 3, 4)
   */
  def apply[T](items: T*): Stack[T] = {
    if(items.isEmpty){
      Empty
    }else{
      NonEmpty(items.head, apply(items.tail: _*))
    }
  }

}
