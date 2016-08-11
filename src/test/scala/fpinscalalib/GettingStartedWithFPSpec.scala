package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class GettingStartedWithFPSpec extends Spec with Checkers {
  def `fibonacci asserts` = {
//    implicit val intArbitrary = Arbitrary[Int](Gen.choose(1, 100))
    val fib: Int => Int = n => {
      @annotation.tailrec def loop(i: Int, prev: Int, cur: Int): Int = if (i <= 0) prev else loop(i - 1, cur, prev + cur)
      loop(n, 0, 1)
    }
    check(Test.testSuccess(GettingStartedWithFPSection.fibAssert _, fib :: HNil))
  }

  def `isSorted asserts` = {
    check(Test.testSuccess(GettingStartedWithFPSection.isSortedAssert _, true :: false :: true :: HNil))
  }

  def `currying asserts` = {
    check(Test.testSuccess(GettingStartedWithFPSection.curryAssert _, true :: true :: HNil))
  }

  def `uncurrying asserts` = {
    check(Test.testSuccess(GettingStartedWithFPSection.uncurryAssert _, true :: true :: HNil))
  }

  def `composing asserts` = {
    check(Test.testSuccess(GettingStartedWithFPSection.composeAssert _, false :: 2 :: 3 :: HNil))
  }
}
