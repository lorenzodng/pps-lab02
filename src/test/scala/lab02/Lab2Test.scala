package lab02

import org.junit.*
import org.junit.Assert.*

class Lab2Test:

  val empty= Lab2.empty
  val isEmpty = Lab2.neg3(empty)
  val select= Lab2.Expr
  val compute= Lab2.Op
  val value= 3
  val expectedSum= 6
  val expectedProduct= 9

  //some tests
  @Test
  def testEmpty(): Unit = {
    val notEmptyString= "hi"
    assertTrue(isEmpty(notEmptyString))
  }

  @Test
  def testIsNotEmpty(): Unit = {
    val emptyString = ""
    assertFalse(isEmpty(emptyString))
  }

  @Test
  def testEvaluateAdd(): Unit = {
    val value = 3
    val addExpression = select.Add(select.Literal(value), select.Literal(value))
    val sum= compute.evaluate(addExpression)
    assertEquals(expectedSum, sum)
  }

  @Test
  def testEvaluateMultiply(): Unit = {
    val value = 3
    val multiplyExpression = select.Multiply(select.Literal(value), select.Literal(value))
    val product = compute.evaluate(multiplyExpression)
    assertEquals(expectedProduct, product)
  }



