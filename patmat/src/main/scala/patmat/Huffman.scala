package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree {
    override def toString = "F(" + left + right + "|" + weight + ")"
  }
  case class Leaf(char: Char, weight: Int) extends CodeTree {
    override def toString = char + "" + weight
  }

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w) => w
    case Fork(left, right, _, w) => weight(left) + weight(right)
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, _) => c :: Nil
    case Fork(left, right, _, _) => chars(left) ::: chars(right)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {

    def timesIter(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) acc
      else if (!isPresent(chars.head, acc)) timesIter(chars.tail, (chars.head, timesCount(chars.head, chars)) :: acc)
      else timesIter(chars.tail, acc)
    }

    def timesCount(char: Char, xs: List[Char]): Int =
      if (xs.isEmpty) 0
      else if (xs.head == char) 1 + timesCount(char, xs.tail)
      else 0 + timesCount(char, xs.tail)

    def isPresent(char: Char, acc: List[(Char, Int)]): Boolean =
      if (acc.isEmpty) false
      else if (acc.head._1 == char) true
      else isPresent(char, acc.tail)

    timesIter(chars, List())
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def makeOrderedLeafListIter(freqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] =
      if (freqs.isEmpty) acc
      else makeOrderedLeafListIter(freqs.tail, insert(freqs.head, acc))

    def insert(pair: (Char, Int), acc: List[Leaf]): List[Leaf] =
      if (acc.isEmpty ||
        pair._2 < acc.head.weight) Leaf(pair._1, pair._2) :: acc
      else acc.head :: insert(pair, acc.tail)

    makeOrderedLeafListIter(freqs, Nil)
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = !trees.isEmpty && trees.tail.isEmpty

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {

    def asLeaf(node: CodeTree) = node.asInstanceOf[Leaf]

    val sorted = sort(trees)

    if (trees.isEmpty || singleton(trees)) trees
    else sorted match {
      case List(left @ Leaf(x1, y1), right @ Leaf(x2, y2), _*) =>
        Fork(left, right, List(x1, x2), y1 + y2) :: sorted.tail.tail

      case List(left @ Fork(p1, q1, r1, s1), right @ Fork(p2, q2, r2, s2), _*) =>
        Fork(left, right, r1 ::: r2, s1 + s2) :: sorted.tail.tail

      case List(left @ Fork(p1, q1, r1, s1), right @ Leaf(x, y), _*) =>
        Fork(left, right, r1 ::: List(x), s1 + y) :: sorted.tail.tail
        
      case List(left @ Leaf(x, y), right @ Fork(p, q, r, s), _*) =>
        Fork(left, right, r ::: List(x), y + s) :: sorted.tail.tail

      case e => throw new Error("Not expected >> " + e)

    }
  }

  def sort(stree: List[CodeTree]): List[CodeTree] = {

    def weight(t: CodeTree): Int = t match {
      case Leaf(x, y) => y
      case Fork(left, right, a, b) => b
    }

    //println("[S]: " +stree)

    def sortIter(stree: List[CodeTree], acc: List[CodeTree]): List[CodeTree] =
      if (stree.isEmpty) acc
      else sortIter(stree.tail, insert(stree.head, acc))

    def insert(t: CodeTree, lst: List[CodeTree]): List[CodeTree] =
      if (lst.isEmpty || weight(t) < weight(lst.head)) t :: lst
      else lst.head :: insert(t, lst.tail)

    sortIter(stree, Nil)

  }

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(singleton: (List[CodeTree] => Boolean), combine: (List[CodeTree] => List[CodeTree]))(trees: List[CodeTree]): List[CodeTree] =
    if (singleton(trees)) trees else until(singleton, combine)(combine(trees))

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val timesSeq = times(chars)
    //println(timesSeq)
    val orderedLeafSeq = makeOrderedLeafList(timesSeq)
    //println(orderedLeafSeq)
    val singletonTree = until(singleton, combine)(orderedLeafSeq)
    //println(singletonTree)
    singletonTree.head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def decodeIter(subtree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
      if (bits.isEmpty) subtree match {
        case Leaf(x, y) => acc ::: List(x)
        case _ => acc
      }
      else
        subtree match {
          case Leaf(x, y) => decodeIter(tree, bits, acc ::: List(x))
          case Fork(left, right, _, _) => bits.head match {
            case 0 => decodeIter(left, bits.tail, acc)
            case 1 => decodeIter(right, bits.tail, acc)
          }
        }
    }

    decodeIter(tree, bits, Nil)

  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {

    def encodeIter(subtree: CodeTree, text: List[Char], acc: List[Bit]): List[Bit] = {
      if (text.isEmpty) acc
      else subtree match {
        case Fork(left, right, chars, w) =>
          if (contains(text.head, left)) encodeIter(left, text, acc ::: List(0))
          else encodeIter(right, text, acc ::: List(1))
        case Leaf(x, y) => if (x == text.head) encodeIter(tree, text.tail, acc)
        else throw new Error("Not found char")
      }
    }

    def contains(char: Char, node: CodeTree): Boolean = node match {
      case Fork(_, _, chars, _) => chars.contains(char)
      case Leaf(x, y) => x == char
    }

    encodeIter(tree, text, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    if (table.isEmpty) throw new Error("Not found char")
    else if (table.head._1 == char) table.head._2
    else codeBits(table.tail)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {

    def convertIter(subtree: CodeTree, acc: List[Bit]): CodeTable = subtree match {
      case Fork(left, right, _, _) => mergeCodeTables(convertIter(left, acc ::: List(0)),
        convertIter(right, acc ::: List(1)))
      case Leaf(x, y) => List((x, acc))
    }

    convertIter(tree, Nil)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {

    def quickEncodeIter(table: CodeTable, text: List[Char], acc: List[Bit]): List[Bit] =
      if (text.isEmpty) acc
      else quickEncodeIter(table, text.tail, acc ::: codeBits(table)(text.head))

    quickEncodeIter(convert(tree), text, Nil)
  }

}
