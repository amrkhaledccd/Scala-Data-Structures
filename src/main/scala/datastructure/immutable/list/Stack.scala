package datastructure.immutable.list

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
  def pop: (T, Stack[T]) = (hd, tl)
  def ++[U >: T](stack: Stack[U]): Stack[U] = NonEmpty(head, tail ++ stack)
  def update[U >: T](t: U, i: Int): Stack[U] = if(i == 0) NonEmpty(t, this) else NonEmpty(head,tail.update(t, i-1))
  def head: T = hd
  def tail: Stack[T] = tl
}

object Stack {

  def apply[T](items: T*): Stack[T] = {
    if(items.isEmpty){
      Empty
    }else{
      NonEmpty(items.head, apply(items.tail: _*))
    }
  }

}
