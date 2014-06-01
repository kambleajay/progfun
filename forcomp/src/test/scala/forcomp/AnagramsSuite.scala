package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
    assert(wordOccurrences("McAdams") === List(('a',2), ('c',1), ('d',1), ('m',2), ('s',1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
    assert(sentenceOccurrences(List("Rachel", "Mcadams", "is", "beautiful")) === 
      List(('a',4), ('b',1), ('c',2), ('d',1), ('e',2), ('f',1), ('h',1), ('i',2), ('l',2), ('m',2), ('r',1), ('s',2), ('t',1), ('u',2)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }
  
  test("subtract: jimmy - my") {
    val jimmy = List(('j', 1), ('i', 1), ('m', 2), ('y', 1))
    val my = List(('m', 1), ('y', 1))
    expect(List(('i',1), ('j',1), ('m',1))) {
      subtract(jimmy, my)
    }
  }
  
  test("subtract: ok - ok") {
    val ok = List(('o', 1), ('k', 1))
    expect(Nil) {
      subtract(ok, ok)
    }
  }
  
  test("subtract: abba - abba") {
    val abba = List(('a', 2), ('b', 2))
    expect(Nil) {
      subtract(abba, abba)
    }
  }
  
  test("subtract: assessment - assess") {
    val assessment = List(('a', 1), ('s', 4), ('e', 2), ('m', 1), ('n', 1), ('t', 1))
    val assess = List(('a', 1), ('s', 4), ('e', 1))
    expect(List(('e',1), ('m',1), ('n',1), ('t',1))) {
      subtract(assessment, assess)
    }
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
  
  test("sentence anagrams: yes man") {
    val sen = List("Yes", "man")
    val ans = List(
         List("en", "as", "my"),
         List("en", "my", "as"),
         List("man", "yes"),
         List("men", "say"),
         List("as", "en", "my"),
         List("as", "my", "en"),
         List("sane", "my"),
         List("Sean", "my"),
         List("my", "en", "as"),
         List("my", "as", "en"),
         List("my", "sane"),
         List("my", "Sean"),
         List("say", "men"),
         List("yes", "man")
       )
  }

}
