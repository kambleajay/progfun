package forcomp

import com.sun.tools.internal.xjc.reader.dtd.Occurence

object Test {

  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  def wordOccurrences(w: Word) = {

    val res = w.toLowerCase.groupBy((x: Char) => x).toList.sorted.map((pair) => (pair._1, pair._2.length))
    res

  }                                               //> wordOccurrences: (w: forcomp.Test.Word)List[(Char, Int)]

  wordOccurrences("McAdams")                      //> res0: List[(Char, Int)] = List((a,2), (c,1), (d,1), (m,2), (s,1))
  def sentenceOccurrences(s: Sentence) = wordOccurrences(s.flatten mkString)
                                                  //> sentenceOccurrences: (s: forcomp.Test.Sentence)List[(Char, Int)]

  sentenceOccurrences(List("Rachel", "Mcadams", "is", "beautiful"))
                                                  //> res1: List[(Char, Int)] = List((a,4), (b,1), (c,2), (d,1), (e,2), (f,1), (h,
                                                  //| 1), (i,2), (l,2), (m,2), (r,1), (s,2), (t,1), (u,2))
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.Test.Word] = List(Aarhus, Aaron, Ababa, aback, ab
                                                  //| aft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased, a
                                                  //| basement, abasements, abases, abash, abashed, abashes, abashing, abasing, ab
                                                  //| ate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, abb
                                                  //| ey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, abb
                                                  //| reviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal, 
                                                  //| abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe, 
                                                  //| abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, abe
                                                  //| rrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorred
                                                  //| , abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, Ab
                                                  //| idjan, Abigail, Abilene, abilities, ability, abject, abjection, abjections, 
                                                  //| abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated, a
                                                  //| blates, ablating, ablation, ablative, ablaze, able, abler, ablest, ably, Abn
                                                  //| er, abnormal, abnormalities, abnormality, abnormally, Abo, aboard, abode, ab
                                                  //| odes, abolish, abolished, abolisher, abolishers, abolishes, abolishing, abol
                                                  //| ishment, abolishments, abolition, abolitionist, abolitionists, abominable, a
                                                  //| bominate, aboriginal, aborigine, aborigines, abort, aborted, aborting, abort
                                                  //| ion, abortions, abortive, abortively, aborts, Abos, abound, abounded, abound
                                                  //| ing, abounds, about, above, aboveboard, aboveground, abovementioned, abrade,
                                                  //|  abraded, abrades, abrading, Abraham, Abram, Abrams, Abramson, abrasion, abr
                                                  //| asions, abrasive, abreaction, abreactions, abreast, abridge, abridged, abrid
                                                  //| ges, abridging, abridgment, abroad, abrogate, abrogated, abrogates, abrogati
                                                  //| ng, abrupt, abruptly, abruptness, abscess, abscessed, abscesses, abscissa, a
                                                  //| bscissas, abscond, absconded, absconding, absconds, absence, absences, absen
                                                  //| t, absented, absentee, absenteeism, absentees, absentia, absenting, absently
                                                  //| , absentminded, absents, absinthe, absolute, absolutely, absoluteness, 
                                                  //| Output exceeds cutoff limit.

  val dictionaryByOccurrences = dictionary.groupBy(w => wordOccurrences(w)) withDefaultValue (List())
                                                  //> dictionaryByOccurrences  : scala.collection.immutable.Map[List[(Char, Int)],
                                                  //| List[forcomp.Test.Word]] = Map(List((c,2), (e,2), (h,1), (r,1), (s,1)) -> Li
                                                  //| st(screech), List((a,2), (l,1), (r,1), (s,1), (t,1)) -> List(altars, astral)
                                                  //| , List((e,1), (f,2), (i,1), (n,1), (s,2), (t,1)) -> List(stiffens), List((b,
                                                  //| 1), (d,1), (e,3), (h,1), (i,1), (k,1), (r,1), (s,1), (w,1)) -> List(bewhiske
                                                  //| red), List((c,1), (d,1), (e,2), (f,1), (i,1), (n,1), (t,1)) -> List(infected
                                                  //| ), List((a,1), (h,1), (k,1), (n,1), (s,1), (t,1)) -> List(thanks), List((c,1
                                                  //| ), (d,1), (e,1), (g,1), (i,1), (m,1), (n,2), (o,1), (t,1), (u,1)) -> List(do
                                                  //| cumenting), List((a,1), (b,1), (e,1), (i,1), (k,1), (n,1), (t,1)) -> List(be
                                                  //| atnik), List((a,1), (d,1), (e,3), (i,1), (m,1), (o,1), (r,1), (s,1), (t,2), 
                                                  //| (v,1)) -> List(overestimated), List((a,1), (e,1), (i,2), (n,1), (p,1), (r,1)
                                                  //| , (s,2), (u,1), (z,1)) -> List(Prussianize), List((a,1), (b,1), (d,1), (e,1)
                                                  //| , (p,1), (s,1), (t,1), (u,1)) -> List(Budapest), List((a,2), (b,1), (e,2), (
                                                  //| l,1), (n,1), (o,1), (s,2)) -> List(seasonable), List((c,1), (e,1), (i,1), (k
                                                  //| ,1), (l,1), (p,1), (s,1)) -> List(pickles), List((a,1), (c,1), (e,1), (i,1),
                                                  //|  (l,1), (n,2), (p,1), (s,1)) -> List(pinnacles), List((d,1), (e,1), (o,1), (
                                                  //| w,1)) -> List(owed), List((a,1), (e,2), (h,1), (l,1), (r,1), (t,1)) -> List(
                                                  //| leather), List((a,2), (c,1), (e,1), (h,1), (i,1), (n,1), (o,1), (r,1), (s,1)
                                                  //| , (t,3), (u,1)) -> List(authenticators), List((d,1), (e,1), (i,2), (m,1), (r
                                                  //| ,1), (s,4)) -> List(dismissers), List((c,1), (d,1), (e,2), (i,1), (v,1)) -> 
                                                  //| List(device), List((e,2), (m,1), (o,1), (r,2), (s,1)) -> List(remorse), List
                                                  //| ((a,2), (d,1), (g,1), (h,1), (i,1), (n,2)) -> List(Gandhian), List((a,2), (e
                                                  //| ,1), (i,2), (n,1), (q,1), (t,3), (u,1), (v,1)) -> List(quantitative), List((
                                                  //| a,1), (e,1), (l,1), (p,1), (r,1), (s,2), (y,1)) -> List(sparsely), List((a,1
                                                  //| ), (b,1), (e,2), (i,2), (l,1), (n,1), (t,1), (v,1)) -> List(inevitable), Lis
                                                  //| t((a,2), (g,1), (m,2), (r,2), (s,1)) -> List(grammars), List((c,1), (e,
                                                  //| Output exceeds cutoff limit.
  val testOccu = List(('a', 1), ('e', 1), ('t', 1))
                                                  //> testOccu  : List[(Char, Int)] = List((a,1), (e,1), (t,1))
  dictionaryByOccurrences(testOccu)               //> res2: List[forcomp.Test.Word] = List(ate, eat, tea)

  def brkup(occurrences: Occurrences) = for ((ch, occ) <- occurrences; count <- 1 to occ) yield (ch, count)
                                                  //> brkup: (occurrences: forcomp.Test.Occurrences)List[(Char, Int)]

  def prod(occlst: List[Occurrences], pair: (Char, Int)) = {
    //println("[OP] " +occlst +" / " +pair)
    occlst ::: List(pair :: Nil) :::
      (for (occr <- occlst; (ch, oc) <- occr; if !occr.exists(_._1 == pair._1)) yield occr ::: (pair :: Nil))
  }                                               //> prod: (occlst: List[forcomp.Test.Occurrences], pair: (Char, Int))List[List[
                                                  //| (Char, Int)]]

  def comb(occurrences: Occurrences) = brkup(occurrences).foldLeft(List[Occurrences]())(prod)
                                                  //> comb: (occurrences: forcomp.Test.Occurrences)List[List[(Char, Int)]]

  def combinations(occurrences: Occurrences) = {
    val res = comb(occurrences)
    res
  }                                               //> combinations: (occurrences: forcomp.Test.Occurrences)List[List[(Char, Int)]
                                                  //| ]

  val occ1 = List(('a', 2), ('b', 2))             //> occ1  : List[(Char, Int)] = List((a,2), (b,2))
  val occ2 = List(('x', 1), ('y', 2), ('z', 3))   //> occ2  : List[(Char, Int)] = List((x,1), (y,2), (z,3))
  brkup(occ1)                                     //> res3: List[(Char, Int)] = List((a,1), (a,2), (b,1), (b,2))
  combinations(occ2)                              //> res4: List[List[(Char, Int)]] = List(List((x,1)), List((y,1)), List((x,1), 
                                                  //| (y,1)), List((y,2)), List((x,1), (y,2)), List((z,1)), List((x,1), (z,1)), L
                                                  //| ist((y,1), (z,1)), List((x,1), (y,1), (z,1)), List((x,1), (y,1), (z,1)), Li
                                                  //| st((y,2), (z,1)), List((x,1), (y,2), (z,1)), List((x,1), (y,2), (z,1)), Lis
                                                  //| t((z,2)), List((x,1), (z,2)), List((y,1), (z,2)), List((x,1), (y,1), (z,2))
                                                  //| , List((x,1), (y,1), (z,2)), List((y,2), (z,2)), List((x,1), (y,2), (z,2)),
                                                  //|  List((x,1), (y,2), (z,2)), List((z,3)), List((x,1), (z,3)), List((y,1), (z
                                                  //| ,3)), List((x,1), (y,1), (z,3)), List((x,1), (y,1), (z,3)), List((y,2), (z,
                                                  //| 3)), List((x,1), (y,2), (z,3)), List((x,1), (y,2), (z,3)))
  def subtract(x: Occurrences, y: Occurrences) = {
    val mapped = (x ++ y).groupBy((x) => x._1)
    def adjust(acc: Occurrences, elem: (Char, Occurrences)) = elem._2 match {
      case x :: Nil => acc ::: (x :: Nil)
      case x :: y :: Nil => acc ::: ((x._1, x._2 - y._2) :: Nil)
    }
    (mapped.foldLeft(List[(Char, Int)]())(adjust)).filter(_._2 > 0).sorted

  }                                               //> subtract: (x: forcomp.Test.Occurrences, y: forcomp.Test.Occurrences)List[(C
                                                  //| har, Int)]
  def possibleWords(occr: Occurrences) = {
    val combs = combinations(occr)
    (for (occ <- combs) yield dictionaryByOccurrences(occ)).toSet.flatten
  }                                               //> possibleWords: (occr: forcomp.Test.Occurrences)scala.collection.immutable.S
                                                  //| et[forcomp.Test.Word]

  def possibleWords1(s: String) = possibleWords(wordOccurrences(s))
                                                  //> possibleWords1: (s: String)scala.collection.immutable.Set[forcomp.Test.Word
                                                  //| ]

  def possibleWords2(occLst: List[Occurrences]) =
    (for {
      occ <- occLst
      comb <- combinations(occ)
    } yield dictionaryByOccurrences(comb)).toSet.flatten
                                                  //> possibleWords2: (occLst: List[forcomp.Test.Occurrences])scala.collection.im
                                                  //| mutable.Set[forcomp.Test.Word]

  def isAnagram1(str1: String, str2: String) =
    subtract(wordOccurrences(str1), wordOccurrences(str2)) == Nil
                                                  //> isAnagram1: (str1: String, str2: String)Boolean

  def isAnagram(str1: String, sen: Sentence) = {
    wordOccurrences(str1) == sentenceOccurrences(sen)
  }                                               //> isAnagram: (str1: String, sen: forcomp.Test.Sentence)Boolean

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagramsHelper(occr: Occurrences): List[Sentence] = {
      if (occr.isEmpty) List(List())
      else (
        for {
          split <- 1 to occr.length

          word <- possibleWords(occr take split)
          rest <- sentenceAnagramsHelper(occr drop split)

          make = word :: rest
          if occr == sentenceOccurrences(make)

        } yield make).toList
    }
    sentenceAnagramsHelper(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: forcomp.Test.Sentence)List[forcomp.Test.Senten
                                                  //| ce]

  def anaWords(str: String, wordSet: Set[Word]) =
    (for {
      split <- 1 to wordSet.size
      words <- wordSet.toList combinations split
      if isAnagram(str, words)
    } yield words)                                //> anaWords: (str: String, wordSet: Set[forcomp.Test.Word])scala.collection.im
                                                  //| mutable.IndexedSeq[List[forcomp.Test.Word]]

  val sen1 = List("Yes", "man")                   //> sen1  : List[java.lang.String] = List(Yes, man)
  val sen2 = List("Linux", "rulez")               //> sen2  : List[java.lang.String] = List(Linux, rulez)
  val senAsStr = sen1 mkString                    //> senAsStr  : String = Yesman
  val split = 5                                   //> split  : Int = 5
  val first = senAsStr take split                 //> first  : String = Yesma
  possibleWords1(senAsStr)                        //> res5: scala.collection.immutable.Set[forcomp.Test.Word] = Set(any, name, ye
                                                  //| as, am, Sam, yes, sea, names, manes, Sean, aye, mean, mane, as, men, man, s
                                                  //| eamy, Mans, Amy, em, nay, May, same, my, easy, me, en, ayes, seam, means, m
                                                  //| ens, yea, amen, say, an, many, sane, Mae, San, Ames)
  val rest = senAsStr drop split                  //> rest  : String = n
  possibleWords1(rest)                            //> res6: scala.collection.immutable.Set[forcomp.Test.Word] = Set()

  val sen1Occrs = sentenceOccurrences(sen1)       //> sen1Occrs  : List[(Char, Int)] = List((a,1), (e,1), (m,1), (n,1), (s,1), (y
                                                  //| ,1))
  val sen1Combs = combinations(sen1Occrs)         //> sen1Combs  : List[List[(Char, Int)]] = List(List((a,1)), List((e,1)), List(
                                                  //| (a,1), (e,1)), List((m,1)), List((a,1), (m,1)), List((e,1), (m,1)), List((a
                                                  //| ,1), (e,1), (m,1)), List((a,1), (e,1), (m,1)), List((n,1)), List((a,1), (n,
                                                  //| 1)), List((e,1), (n,1)), List((a,1), (e,1), (n,1)), List((a,1), (e,1), (n,1
                                                  //| )), List((m,1), (n,1)), List((a,1), (m,1), (n,1)), List((a,1), (m,1), (n,1)
                                                  //| ), List((e,1), (m,1), (n,1)), List((e,1), (m,1), (n,1)), List((a,1), (e,1),
                                                  //|  (m,1), (n,1)), List((a,1), (e,1), (m,1), (n,1)), List((a,1), (e,1), (m,1),
                                                  //|  (n,1)), List((a,1), (e,1), (m,1), (n,1)), List((a,1), (e,1), (m,1), (n,1))
                                                  //| , List((a,1), (e,1), (m,1), (n,1)), List((s,1)), List((a,1), (s,1)), List((
                                                  //| e,1), (s,1)), List((a,1), (e,1), (s,1)), List((a,1), (e,1), (s,1)), List((m
                                                  //| ,1), (s,1)), List((a,1), (m,1), (s,1)), List((a,1), (m,1), (s,1)), List((e,
                                                  //| 1), (m,1), (s,1)), List((e,1), (m,1), (s,1)), List((a,1), (e,1), (m,1), (s,
                                                  //| 1)), List((a,1), (e,1), (m,1), (s,1)), List((a,1), (e,1), (m,1), (s,1)), Li
                                                  //| st((a,1), (e,1), (m,1), (s,1)), List((a,1), (e,1), (m,1), (s,1)), List((a,1
                                                  //| ), (e,1), (m,1), (s,1)), List((n,1), (s,1)), List((a,1), (n,1), (s,1)), Lis
                                                  //| t((a,1), (n,1), (s,1)), List((e,1), (n,1), (s,1)), List((e,1), (n,1), (s,1)
                                                  //| ), List((a,1), (e,1), (n,1), (s,1)), List((a,1), (e,1), (n,1), (s,1)), List
                                                  //| ((a,1), (e,1), (n,1), (s,1)), List((a,1), (e,1), (n,1), (s,1)), List((a,1),
                                                  //|  (e,1), (n,1), (s,1)), List((a,1), (e,1), (n,1), (s,1)), List((m,1), (n,1),
                                                  //|  (s,1)), List((m,1), (n,1), (s,1)), List((a,1), (m,1), (n,1), (s,1)), List(
                                                  //| (a,1), (m,1), (n,1), (s,1)), List((a,1), (m,1), (n,1), (s,1)), List((a,1), 
                                                  //| (m,1), (n,1), (s,1)), List((a,1), (m,1), (n,1), (s,1)), List((a,1), (m,1), 
                                                  //| (n,1), (s,1)), List((e,1), (m,1), (n,1), (s,1)), List((e,1), (m,1), (n,1), 
                                                  //| (s,1)), List((e,1), (m,1), (n,1), (s,1)), List((e,1), (m,1), (n,1), (s,1)),
                                                  //|  List((e,1), (m,1), (n,1), (s,1)), List((e,1), (m,1), (n,1), (s,1)), List((
                                                  //| a,1), (e,1), (m,1), (n,1), (s,1)), List((a,1), (e,1), (m,1), (n,1), (s
                                                  //| Output exceeds cutoff limit.

  def whatIs(senOccr: Occurrences): List[Sentence] =
    if (senOccr.isEmpty) List(List())
    else (for {
      aComb <- combinations(senOccr)
      wordForComb <- dictionaryByOccurrences(aComb)
      restOccr = subtract(senOccr, wordOccurrences(wordForComb))
      restWords <- whatIs(restOccr)
    } yield wordForComb :: restWords)             //> whatIs: (senOccr: forcomp.Test.Occurrences)List[forcomp.Test.Sentence]

  whatIs(sen1Occrs).toSet                         //> res7: scala.collection.immutable.Set[forcomp.Test.Sentence] = Set(List(as, 
                                                  //| en, my), List(my, Sean), List(my, en, as), List(sane, my), List(my, sane), 
                                                  //| List(say, men), List(man, yes), List(en, as, my), List(men, say), List(yes,
                                                  //|  man), List(as, my, en), List(my, as, en), List(Sean, my), List(en, my, as)
                                                  //| )
  whatIs(sentenceOccurrences(sen2)).toSet size    //> res8: Int = 20

  //val sen1Combs = combinations(sen1Occrs)

  //sentenceAnagrams(List("Linux", "Rulez")) mkString "\n"
  //sentenceAnagrams(List("Mickey", "Mouse"))
  
  val abba = List(('a', 2), ('b', 2))             //> abba  : List[(Char, Int)] = List((a,2), (b,2))
  def combinations1(occurrences: Occurrences): List[Occurrences] =
    if (occurrences.isEmpty) List(List())
    else
      for {
        (firstChar, firstCount) <- occurrences
        (restChar, restCount) <- occurrences.tail
        //(restChar, restCount) <- restOccrLst
        countIter1 <- 1 to firstCount
        countIter2 <- 1 to restCount
        if firstChar < restChar
      } yield (firstChar, countIter1) :: (restChar, countIter2) :: Nil
                                                  //> combinations1: (occurrences: forcomp.Test.Occurrences)List[forcomp.Test.Occ
                                                  //| urrences]
  	
  combinations1(abba) mkString "\n"               //> res9: String = List((a,1), (b,1))
                                                  //| List((a,1), (b,2))
                                                  //| List((a,2), (b,1))
                                                  //| List((a,2), (b,2))
}