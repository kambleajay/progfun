package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Fork(Leaf('z', 5), Leaf('x', 2), List('z', 'x'), 7)

    val a = Leaf('a', 9); val b = Leaf('b', 3); val c = Leaf('c', 1); val d = Leaf('d', 1); val e = Leaf('e', 1); val f = Leaf('f', 1); val g = Leaf('g', 1); val h = Leaf('h', 1)
    val cd = Fork(c, d, List('c', 'd'), 2)
    val ef = Fork(e, f, List('e', 'f'), 2)
    val gh = Fork(g, h, List('g', 'h'), 2)
    val efgh = Fork(ef, gh, List('e', 'f', 'g', 'h'), 4)
    val bcd = Fork(cd, b, List('b', 'c', 'd'), 5)
    val bcdefgh = Fork(efgh, bcd, List('b', 'c', 'd', 'e', 'f', 'g', 'h'), 9)
    val abcdefgh = Fork(a, bcdefgh, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), 17)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      expect(9) {
        weight(t2)
      }
      expect(7) {
        weight(t3)
      }
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
      expect(List('a', 'b')) {
        chars(t1)
      }
      expect(List('z', 'x')) {
        chars(t3)
      }
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  /*ignore("times: isPresent") {
    expect(true) {
      isPresent('a', List(('b', 3), ('a', 2)))
    }
    expect(false) {
      isPresent('x', List(('b', 3), ('a', 2)))
    }
    expect(true) {
      isPresent('b', List(('b', 3), ('a', 2)))
    }
  }
  
  ignore("times: count") {
    expect(2) {
      timesCount('a', List('a', 'b', 'b', 'b', 'a'))
    }
    expect(1) {
      timesCount('x', List('x'))
    }
    expect(0) {
      timesCount('z', List('x'))
    }
  }
  
  ignore("times: iter") {
    expect(List(('x', 2), ('y', 1), ('z', 3))) {
      timesIter(List('x', 'y', 'z', 'z', 'x', 'z'), Nil)
    }
    expect(List(('a', 2), ('b', 1))) {
      timesIter(List('a', 'b', 'a'), Nil)
    }
  }*/

  test("times should return correct no of occurences") {
    expect(List(('z', 3), ('y', 1), ('x', 2))) {
      times(List('x', 'y', 'z', 'z', 'x', 'z'))
    }
    expect(List(('b', 1), ('a', 2))) {
      times(List('a', 'b', 'a'))
    }
  }

  /*test("insert at correct pos") {
    expect(List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3))) {
      insert(('b', 2), List(Leaf('a', 1), Leaf('c', 3)))
    }
    expect(List(Leaf('a', 1), Leaf('b', 1), Leaf('c', 3))) {
      insert(('b', 1), List(Leaf('a', 1), Leaf('c', 3)))
    }
    expect(List(Leaf('a', 1), Leaf('b', 1), Leaf('c', 3), Leaf('z', 4))) {
      insert(('z', 4), List(Leaf('a', 1), Leaf('b', 1), Leaf('c', 3)))
    }
  }*/

  test("makeOrderedLeafList returns list of leaf nodes sorted by asec weights") {
    expect(List(Leaf('y', 1), Leaf('x', 2), Leaf('z', 3))) {
      makeOrderedLeafList(List(('z', 3), ('y', 1), ('x', 2)))
    }
    expect(List(Leaf('b', 1), Leaf('a', 2))) {
      makeOrderedLeafList(List(('b', 1), ('a', 2)))
    }
    expect(Nil) {
      makeOrderedLeafList(Nil)
    }
    expect(List(Leaf('y', 1), Leaf('z', 1), Leaf('x', 1))) {
      makeOrderedLeafList(List(('y', 1), ('z', 1), ('x', 1)))
    }
    expect(List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3))) {
      makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
    }
  }

  ignore("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton returns true for single node tree") {
    expect(true) {
      singleton(List(Leaf('a', 1)))
    }
    expect(true) {
      singleton(List(Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3)))
    }
    expect(false) {
      singleton(Nil)
    }
    expect(false) {
      singleton(List(Leaf('a', 1), Leaf('b', 2)))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    expect(Nil) {
      combine(Nil)
    }
  }

  test("create code tree") {
    new TestTrees {
      expect(Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)) {
        createCodeTree("ddddbbbaa".toList)
      }
    }
  }

  test("encode") {
    new TestTrees {
      expect(List(1, 0, 1, 0, 0)) {
        encode(t2)("dba".toList)
      }
    }
  }

  test("decode") {
    new TestTrees {
      expect("dba".toList) {
        decode(t2, List(1, 0, 1, 0, 0))
      }
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("secret msg") {
    println(decodedSecret)
  }

  test("convert") {
    new TestTrees {
      expect(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))) {
        convert(t2)
      }
    }
  }
  
  test("quickEncode") {
    new TestTrees {
      expect(List(0, 0, 0, 1, 1, 1, 0, 1, 0, 0)) {
        quickEncode(t2)("abddba".toList)
      }
    }
  }
}
