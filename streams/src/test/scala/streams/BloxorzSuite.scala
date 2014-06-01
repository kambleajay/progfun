package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooooooo
        |ooooooo
        |ooooooo
        |oooSooo
        |ooooooo
        |oooooTo
        |ooooooo""".stripMargin

  }

  trait Level3 extends SolutionChecker {

    val level =
      """------ooooooo--
        |oooo--ooo--oo--
        |ooooooooo--oooo
        |oSoo-------ooTo
        |oooo-------oooo
        |------------ooo""".stripMargin
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(5, 8)))
      assert(terrain(Pos(3, 9)))
      assert(terrain(Pos(2, 7)))
      assert(terrain(Pos(1, 5)))
      assert(terrain(Pos(3, 1)))

      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(3, 0)))
      assert(!terrain(Pos(4, 4)))
      assert(!terrain(Pos(1, 9)))
      assert(!terrain(Pos(5, 5)))
      assert(!terrain(Pos(5, 9)))
      assert(!terrain(Pos(0, 8)))
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1, 1))
      assert(goal === Pos(4, 7))
    }
  }

  test("block: starting block") {
    new Level1 {
      expect(Block(Pos(1, 1), Pos(1, 1))) {
        startBlock
      }
    }
    new Level3 {
      expect(Block(Pos(3, 1), Pos(3, 1))) {
        startBlock
      }
    }
  }

  test("block: isStanding test") {
    new Level1 {
      assert(startBlock.isStanding == true)
      val blockAtMove1 = startBlock.right
      expect(false) {
        blockAtMove1.isStanding
      }
      val blockAtMove2 = blockAtMove1.right
      expect(true) {
        blockAtMove2.isStanding
      }
    }

    new Level3 {
      assert(startBlock.isStanding === true)
      val afterMove1 = startBlock.right
      assert(afterMove1.isStanding === false)
      val afterMove2 = afterMove1.up
      assert(afterMove2.isStanding === false)
      val afterMove3 = afterMove2.right
      assert(afterMove3.isStanding === true)
    }
  }

  test("block: isLegal test") {
    new Level1 {
      expect(true) { startBlock.isLegal }
      val move1 = startBlock.up
      expect(false) { move1.isLegal }
      val move2 = startBlock.left
      expect(false) { move2.isLegal }
      val move3 = startBlock.down
      expect(true) { move3.isLegal }
    }

    new Level3 {
      expect(true) { startBlock.isLegal }
      val move1 = startBlock.down
      expect(false) { move1.isLegal }
      val move2 = startBlock.left
      expect(false) { move2.isLegal }
      val move3 = startBlock.up
      expect(true) { move3.isLegal }
    }
  }

  test("block: neighbors") {
    new Level2 {
      /*expect(List((Block(Pos(0, 1), Pos(-1, 1)), Up), 
    		  (Block(Pos(1, 2), Pos(1, 3)), Right), 
    		  (Block(Pos(2, 1), Pos(3, 1)), Down), 
    		  (Block(Pos(1, -1), Pos(1, 0)), Left))) {
        startBlock neighbors
      }
      
      intercept[IllegalArgumentException] {
        startBlock neighbors
      }*/

      expect(List((Block(Pos(1, 3), Pos(2, 3)), Up),
        (Block(Pos(3, 4), Pos(3, 5)), Right),
        (Block(Pos(4, 3), Pos(5, 3)), Down),
        (Block(Pos(3, 1), Pos(3, 2)), Left))) {
        startBlock neighbors
      }
    }
  }

  test("block: legal neighbors") {
    new Level2 {
      expect(List((Block(Pos(1, 3), Pos(2, 3)), Up),
        (Block(Pos(3, 4), Pos(3, 5)), Right),
        (Block(Pos(4, 3), Pos(5, 3)), Down),
        (Block(Pos(3, 1), Pos(3, 2)), Left))) {
        startBlock legalNeighbors
      }
    }

    new Level1 {
      expect(List(
        (Block(Pos(1, 2), Pos(1, 3)), Right),
        (Block(Pos(2, 1), Pos(3, 1)), Down))) {
        startBlock legalNeighbors
      }
    }

    new Level3 {
      expect(List(
        (Block(Pos(1, 1), Pos(2, 1)), Up),
        (Block(Pos(3, 2), Pos(3, 3)), Right))) {
        startBlock legalNeighbors
      }
    }
  }

  test("solution step: done") {
    new Level1 {
      expect(true) { done(Block(goal, goal)) }
      expect(false) { done(startBlock right) }
      expect(false) { done(startBlock down) }
    }

    new Level3 {
      expect(true) { done(Block(goal, goal)) }
      expect(false) { done(startBlock right) }
      expect(false) { done(startBlock up) }
    }
  }

  test("solution step: neighborsWithHistory") {
    new Level1 {
      expect(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))) {
        neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).take(2).toSet
      }

      expect(Set(
        (Block(Pos(2, 4), Pos(3, 4)), List(Down, Right, Right)),
        (Block(Pos(1, 2), Pos(1, 3)), List(Left, Right, Right)))) {
        neighborsWithHistory(Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)).take(2).toSet
      }
    }
  }

  test("solution step: newNeighborsOnly") {
    new Level1 {
      expect(Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))) {
        newNeighborsOnly(
          Set(
            (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
            (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

          Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))).take(3).toSet
      }

      expect(Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)),
        (Block(Pos(1, 3), Pos(2, 3)), List(Up, Up, Up)))) {
        newNeighborsOnly(
          Set(
            (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
            (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)),
            (Block(Pos(1, 3), Pos(2, 3)), List(Up, Up, Up))).toStream,

          Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))).take(4).toSet
      }
    }
  }
  
  test("solution step: from") {
    new Level1 {
      expect(Nil) {
        from(Stream.cons((startBlock, Nil), Nil), Nil)
      }
    }
  }

  ignore("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  ignore("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
