package datastructure.immutable.list

import scala.annotation.tailrec


case object Nil extends LinkedList[Nothing]

case class ::[+A](hd: A, tl: LinkedList[A]) extends LinkedList[A]

object LinkedList {

  /*
     Initializes LinkedList
     val linkedList = LinkedList(1, 2, 3, 4, 5)
   */
  def apply[A](items: A*): LinkedList[A] = {
    if(items.isEmpty) Nil
    else ::(items.head, apply(items.tail: _*))
  }
}

class LinkedList[+A] {

  /*
    returns true if the list is empty
   */
  def isEmpty: Boolean = this match {
      case Nil => true
      case _ => false
  }

  /*
    returns the first element in the list
   */
  def head: A = this match {
    case Nil => throw new NoSuchElementException
    case h :: _ => h
  }

  /*
    returns the list elements without the head (first element)
   */
  def tail: LinkedList[A] = this match  {
    case Nil => throw new NoSuchElementException
    case _ :: t => t
  }

  /*
    Gets an element at index
   */
  def get(index: Int): A = {

    @tailrec
    def loop(list: LinkedList[A], index: Int): A =  (list, index) match {
        case (Nil, _) => throw new IndexOutOfBoundsException
        case (_, i) if i < 0 => throw new IndexOutOfBoundsException
        case (h :: _, 0) => h
        case (_ :: t, i) => loop(t, i - 1)
    }

    loop(this, index)
  }

  /*
    Reverses the current(this) list
   */
  def reverse: LinkedList[A] =  {

    @tailrec
    def loop(accumulator: LinkedList[A], list: LinkedList[A]): LinkedList[A] = list match {
      case Nil => accumulator
      case (h :: t) => loop(::(h, accumulator), t)
    }

    loop(Nil, this)
  }

  /*
  Drops nth elements from the beginning of the current list (this)
   */
  def drop(count: Int): LinkedList[A] = {

    @tailrec
    def loop(list: LinkedList[A], count: Int): LinkedList[A] = (list, count) match {
      case (Nil, _) => Nil
      case (_ :: t, 1)  => t
      case (l, i) if i <= 0  => l
      case (_ :: t, c) => loop(t, c - 1)
    }

    loop(this, count)
  }

  /*
    Drops the elements from the current list (this) while the condition evaluates to true
   */
  def dropWhile(f: A => Boolean): LinkedList[A] = {

    @tailrec
    def loop(list: LinkedList[A], f: A => Boolean): LinkedList[A] = list match {
      case Nil => Nil
      case (h :: t) if f(h) => loop(t, f)
      case l => l
    }

    loop(this, f)
  }

  /*
    Adds new element (value) at the beginning of the current list (this)
   */
  def prepend[B >: A](value: B): LinkedList[B] = this match {
    case Nil => ::(value, Nil)
    case l => ::(value, l)
  }

  /*
    Adds "that" list at the beginning of current (this) list
   */
  def prependAll[B >: A](that: LinkedList[B]): LinkedList[B] = {

    @tailrec
    def loop(list: LinkedList[B], that: LinkedList[B]): LinkedList[B] = that match {
      case Nil => list
      case h :: t => loop(list.prepend(h), t)
    }

    loop(this, that.reverse)
  }

  /*
    Adds "that" list in front of current (this) list
   */
  def ++[B >: A] (that: LinkedList[B]): LinkedList[B] = {
    that prependAll this
  }

  /*
    Adds an element in front of current list (this)
   */
  def append[B >: A](value: B): LinkedList[B] = {
    this ++ LinkedList(value)
  }

  /*
    Deletes element at index
   */
  def delete(index: Int): LinkedList[A] = {

    @tailrec
    def loop(list: LinkedList[A],accumulator: LinkedList[A], idx: Int): LinkedList[A] = (list, idx) match {
      case (Nil, _) => throw new ArrayIndexOutOfBoundsException
      case (_, i) if i < 0 => throw new ArrayIndexOutOfBoundsException
      case (_ :: t, 0) => t prependAll accumulator.reverse
      case (h :: t, i) => loop(t, ::(h, accumulator), i - 1)
    }

    loop(this, Nil, index)
  }

}
