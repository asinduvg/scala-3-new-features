package com.rockthejvm.part1changes

object ExtensionMethods {

  /*
    implicit vals/arguments -> given/using
    implicit classes -> extension methods
   */

  // Scala 2
  implicit class MyRichInteger(number: Int) {
    def isEven: Boolean = number % 2 == 0
  }

  val is2Even = 2.isEven // new MyRichInteger(2).isEven

  // Scala 3: extension method
  extension (number: Int)
    def isEven_v2: Boolean = number % 2 == 0
  // {} not necessary if we use significant indentation

  val is2Even_v2 = 2.isEven_v2

  // recommended: use extension methods

  // generic extensions
  extension [A](list: List[A])
    def ends: (A, A) = (list.head, list.last)

  val aList = List(1, 2, 3, 4)
  val firstLast = aList.ends

  /*
    reason 1: make APIs very expressive
    reason 2: enhance CERTAIN types with new methods BUT NOT others
   */

  // type class
  trait SemiGroup[A]:
    def combine(x: A, y: A): A

  given intSemigroup: SemiGroup[Int] with
    override def combine(x: Int, y: Int): Int = x + y

  extension [A](list: List[A])
    def combineAll(using semigroup: SemiGroup[A]): A =
      list.reduce(semigroup.combine)

  val sum = aList.combineAll
  val listOfStrings = List("black", "white")
  //  val combinationOfStrings = listOfStrings.combineAll // doesn't work

  // grouping extensions
  object GroupedExtensions:
    extension [A](list: List[A])
      def ends: (A, A) = (list.head, list.last)
      def combineAll(using semigroup: SemiGroup[A]): A =
        list.reduce(semigroup.combine)
    end extension
  end GroupedExtensions

  /**
   * Exercises:
   * 1. Replace an implicit class with an extension clause
   * 2. Add extension methods for a binary tree
   *    - map(f: A => B): Tree[B]
   *    - forAll(predicate: A => Boolean): Boolean
   *    - sum => sum of all the elements of a INTEGER tree
   */

  implicit class PrimeChecker(n: Int) {
    def isPrime: Boolean = {
      def isPrimeHelper(potDivisor: Int): Boolean =
        if (potDivisor > n / 2) true
        else isPrimeHelper(potDivisor + 1)

      assert(n >= 0)
      if (n == 0 || n == 1) false
      else isPrimeHelper(2)
    }
  }

  extension (n: Int)
    def isPrime_v2: Boolean =
      def isPrimeHelper(potDivisor: Int): Boolean =
        if (potDivisor > n / 2) true
        else isPrimeHelper(potDivisor + 1)

      assert(n >= 0)
      if (n == 0 || n == 1) false
      else isPrimeHelper(2)
  end extension

  // 2
  sealed abstract class Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  trait SummingMethod[A]:
    def summingMethod(x: A, y: A): A

  given intSummingMethod: SummingMethod[Int] with
    override def summingMethod(x: Int, y: Int): Int = x + y

  extension [A](tree: Tree[A])
    def map[B](f: A => B): Tree[B] = tree match
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) =>
        Branch(left.map(f), right.map(f))
    end map

    def forAll(predicate: A => Boolean): Boolean = tree match
      case Leaf(value) => predicate(value)
      case Branch(left, right) =>
        left.forAll(predicate) && right.forAll(predicate)
    end forAll

    def sum(using summingMethod: SummingMethod[A]): A = tree match
      case Leaf(value) => value
      case Branch(left, right) =>
        summingMethod.summingMethod(left.sum, right.sum)
    end sum
  end extension

  // another approach to Sum of Int
  //  extension (tree: Tree[Int])
  //    def sum: Int = tree match
  //      case Leaf(value) => value
  //      case Branch(left, right) => left.sum + right.sum

  def main(args: Array[String]): Unit =
    val myTree = Branch(Branch(Leaf(4), Leaf(20)), Leaf(6))
    println(myTree.map(_ * 10).sum)
}
