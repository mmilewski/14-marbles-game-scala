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
      (3 to 5) map (i => Pos("e", i) -> EmptyBoardPiece),
      (2 to 5) map (i => Pos("d", i) -> EmptyBoardPiece),
      (1 to 5) map (i => Pos("c", i) -> EmptyBoardPiece),
      (1 to 4) map (i => Pos("b", i) -> EmptyBoardPiece),
      (1 to 3) map (i => Pos("a", i) -> EmptyBoardPiece)
    ).flatten.toMap
  )

  val player1 = Player("Tom", "white")
  val player2 = Player("Jerry", "black")

  def createGame(board: Board): Game = new Game(board)

  "When the player wants to move a marble around, he should" should {

    "be forbidden to move a marble he does not own" in {
      val src = Pos("a", 1)
      val dst = Pos("a", 2)

      val board: Board = emptyBoard.updated(src, player1.marble)

      // verify
      val newGame = createGame(board).move(player2, src, dst)
      newGame must beFailedTry.withThrowable[IllegalMoveException]
    }

    "be forbidden to move a marble to nonempty field" in {
      val src = Pos("a", 1)
      val dst = Pos("a", 2)

      val board: Board = emptyBoard.updated(src, player1.marble).updated(dst, player2.marble)

      // verify
      val newGame = createGame(board).move(player1, src, dst)
      newGame must beFailedTry.withThrowable[IllegalMoveException]
    }

    "be allowed to move his marble to all neighbour fields and nowhere else" in {
      val src = Pos("b", 2)
      val marble = player1.marble

      val board: Board = emptyBoard.updated(src, marble)

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

  "When the player wants to move multiple marbles, he should" should {

    "be able to move 2 horizontally" in {
      val pos1 = Pos("b", 1)
      val pos2 = Pos("b", 2)
      val pos3 = Pos("b", 3)

      val marble = player1.marble

      val board = emptyBoard
        .updated(pos1, marble)
        .updated(pos2, marble)

      val newGame = createGame(board).moveMany(player1, List(pos1, pos2), MoveDirection.East)
      newGame must beSuccessfulTry.like {
        case game: Game =>
          val newBoard: Board = game.board
          newBoard.at(pos1) === EmptyBoardPiece
          newBoard.at(pos2) === marble
          newBoard.at(pos3) === marble
          success
      }

    }

    "be able to move 3 horizontally" in {
      success
    }

    "be forbidden to move 4 horizontally" in {
      success
    }

    "be forbidden to move vertically if there is not enough space" in {
      success
    }
  }

  "When the player wants to push other player's marbles, he should" should {

    "be able to push 1 marble with 2 marbles" in {
      success
    }

    "be able to push 2 marbles with 3 marbles" in {
      success
    }

    "be forbidden to push 1 marble" in {
      success
    }

    "be forbidden to push his marbles" in {
      success
    }

    "be forbidden to push with other player's marbles" in {
      success
    }

    "be forbidden to push if there is not enough empty space" in {
      success
    }

  }

}
