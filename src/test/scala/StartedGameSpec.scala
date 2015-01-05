import core._
import org.specs2.mutable.SpecificationLike

class StartedGameSpec extends SpecificationLike {
  sequential

  val boardLines = """
                     |  e o o o
                     | d o o o o
                     |c o o o o o
                     | b o o o o 5
                     |  a o o o 4
                     |     1 2 3
                   """
  val emptyBoard = Board(
    Seq(
      (3 to 5) map (i => Pos("e", i) -> NoPiece),
      (2 to 5) map (i => Pos("d", i) -> NoPiece),
      (1 to 5) map (i => Pos("c", i) -> NoPiece),
      (1 to 4) map (i => Pos("b", i) -> NoPiece),
      (1 to 3) map (i => Pos("a", i) -> NoPiece)
    ).flatMap(identity).toMap
  )

  val player1 = Player("Tom", "white")
  val player2 = Player("Jerry", "black")

  def createGame(board: Board): Game = new Game(board)

  "When the player wants to move his marble around, he should" should {

    "be forbidden to move a marble he does not own" in {
      val src = Pos("a", 1)
      val dst = Pos("a", 2)

      val board: Board = emptyBoard.overwrite(src, player1.marble)

      // verify
      val newGame = createGame(board).move(player2, src, dst)
      newGame must beFailedTry.withThrowable[IllegalMoveException]
    }

    "be forbidden to move a marble to nonempty field" in {
      val src = Pos("a", 1)
      val dst = Pos("a", 2)

      val board: Board = emptyBoard.overwrite(src, player1.marble).overwrite(dst, player2.marble)

      // verify
      val newGame = createGame(board).move(player1, src, dst)
      newGame must beFailedTry.withThrowable[IllegalMoveException]
    }

    "be allowed to move his marble to all neighbour fields and nowhere else" in {
      val src = Pos("b", 2)
      val marble = player1.marble

      val board: Board = emptyBoard.overwrite(src, marble)

      // verify
      val allowedPositions = for {
        row <- List("a", "b", "c", "d", "e")
        col <- 1 to 5
        p = Pos(row, col)
        newGame = createGame(board).move(player1, src, p)
        if newGame.isSuccess
      } yield {
        p
      }

      allowedPositions must containTheSameElementsAs(List(
        Pos("b", 1), Pos("b", 3),
        Pos("a", 1), Pos("a", 2),
        Pos("c", 2), Pos("c", 3)
      ))
    }

  }
}
