package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */
  
  def mkSet(l: List[Int]): Set = if(l.size == 1) singletonSet(l.head) else union(singletonSet(l.head), mkSet(l.tail))

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = mkSet(List(1, 2, 3))
    val s5 = mkSet(List(1, -2, -3))
    val s6 = mkSet(List(-1, -2, -3))
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet tests") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
      assert(!contains(s3, 1), "Singleton 3 does not have 1")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersection tests") {
    new TestSets {
      val s = intersect(s4, s5)
      assert(contains(s, 1))
      assert(!contains(s, 2))
      assert(!contains(s, 3))
    }
  }
  
  test("diff tests") {
    new TestSets {
      val s = diff(s4, s5)
      assert(contains(s, 2))
      assert(contains(s, 3))
      assert(!contains(s, 1))
    }
  }
  
  test("filter tests") {
    new TestSets {
      val s = filter(s5, _ < 0)
      assert(contains(s, -2))
      assert(contains(s, -3))
      assert(!contains(s, 1))
    }
  }
  
  test("forall tests") {
    new TestSets {
      assert(forall(s5, _ != 0))
      assert(forall(s5, _ < 4))
      assert(forall(s6, _ < 0))
    }
  }
  
  test("exists tests") {
    new TestSets {
      assert(exists(s1, _ == 1))
      assert(exists(s6, _ < 0))
      assert(!exists(s4, _ < 0))
    }
  }
  
  test("map tests") {
    new TestSets {
      val s = map(s4, x => x * x)
      assert(contains(s, 1))
      assert(contains(s, 4))
      assert(contains(s, 9))
      assert(!contains(s, 2))
    }
  }
}
