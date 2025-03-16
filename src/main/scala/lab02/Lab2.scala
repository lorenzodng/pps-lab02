package lab02

import lab02.Lab2.Expr.{Add, Literal, Multiply}

object Lab2 extends App :

  // Task 1, svolto da solo
  def mult(x: Double, y: Double): Double = x * y

  def curriedMult(x: Double)(y: Double): Double = x * y

  def divide(x: Int, y: Int): Int = x / y

  def curriedDivide(x: Int)(y: Int): Int = x / y

  // tests
  println(mult(11, 2))

  val multiplyBy3 = curriedMult(3)
  val multiplyBy2 = multiplyBy3(2)
  println(multiplyBy2)

  println(divide(20, 2))

  val numerator = curriedDivide(6)
  val divideBy3 = numerator(3)
  println(divideBy3)

  // Task 2, svolto da solo
  // 3.
  // a)
  val f: Int => String = _ match
    case x if x >= 0 => "positive"
    case _ => "negative"

  def f2(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  // b)
  val empty: String => Boolean = _ match
    case s if s == "" => true
    case _ => false

  val neg: (String => Boolean) => String => Boolean = pred => s => pred(s) match
    case true => false
    case false => true

  def neg2(pred: String => Boolean): String => Boolean = s => pred(s) match
    case true => false
    case false => true

  // c)
  def neg3[A](pred: A => Boolean): A => Boolean = s => pred(s) match
    case true => false
    case false => true

  // 4.
  val nonCurriedFun: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  val curriedFun: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  def nonCurriedFun2(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  def curriedFun2(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  // 5.
  def compose[A](f: A => A, g: A => A): A => A = (x: A) => f(g(x))

  // 6.
  def composeThree[A, B, C, D](f: A => B, g: B => C, h: C => D): A => D = (x: A) => h(g(f(x)))

  // tests
  println(f(1))
  println(f2(-1))

  val notEmpty = neg(empty)
  println(notEmpty("hi"))
  println(notEmpty(""))

  val notEmpty2 = neg2(empty)
  println(notEmpty2("hi"))
  println(notEmpty2(""))

  val notEmpty3 = neg3(empty)
  println(notEmpty3("hi"))
  println(notEmpty3(""))

  println(nonCurriedFun(3, 3, 3))

  val partialFunX = curriedFun(3)
  val partialFunY = partialFunX(3)
  println(partialFunY(4))

  println(nonCurriedFun2(3, 3, 3))

  val partialFunX2 = curriedFun2(3)
  val partialFunY2 = partialFunX2(3)
  println(partialFunY2(4))

  val add: Int => Int = x => x + 1
  val multiply: Int => Int = x => x * 2
  val addMultiply = compose(add, multiply)
  println(addMultiply(2))

  val subtract: Int => Int = x => x - 1
  val addSubtractMultiply = composeThree(add, subtract, multiply)
  println(addSubtractMultiply(2))

  // Task 3, svolto da solo
  // 7.
  def power(base: Double, exponent: Int): Double = exponent match
    case exponent if exponent == 0 => 1
    case _ => base * power(base, exponent - 1)

  def power2(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def powerTail(exponent: Double, acc: Double): Double = exponent match
      case exponent if exponent == 0 => acc
      case _ => powerTail(exponent - 1, base * acc)

    powerTail(exponent, 1)

  // 8.
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def reverseTail(n: Int, acc: Int): Int = n match
      case n if (n / 10) == 0 && (n % 10) == 0 => acc
      case _ => reverseTail(n / 10, (acc * 10) + (n % 10))

    reverseTail(n, 0)

  // tests
  println(power(2, 3))

  println(power2(2, 3))

  println(reverseNumber(12345))

  // Task 4, svolto da solo
  // 9.
  enum Expr:
    case Literal(x: Int)
    case Add(x: Expr, y: Expr)
    case Multiply(x: Expr, y: Expr)

  object Op:
    def evaluate(expr: Expr): Int = expr match
      case Literal(value) => value
      case Add(x, y) => evaluate(x) + evaluate(y)
      case Multiply(x, y) => evaluate(x) * evaluate(y)

    def show(expr: Expr): String = expr match
      case Literal(value) => "Value: " + value
      case Add(x, y) => "Values: " + evaluate(x) + ", " + evaluate(y)
      case Multiply(x, y) => "Values: " + evaluate(x) + ", " + evaluate(y)

  //tests
  val addExpression= Add(Expr.Literal(2), Expr.Literal(1))
  println(Op.evaluate(addExpression))

  val multiplyExpression = Multiply(Expr.Literal(2), Expr.Literal(1))
  println(Op.evaluate(multiplyExpression))



