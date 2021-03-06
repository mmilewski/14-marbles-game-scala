import core._
import org.scalatest.{Inside, MustMatchers, TryValues, WordSpec}

import scala.util.Try

class StartedGameScalaTest extends WordSpec with TryValues with MustMatchers with Inside {

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

  "During the game" when {
    "player wants to move a marble around, he should" should {
      "be forbidden to move a marble he does not own" in {
        val src = Pos("a", 1)
        val dst = Pos("a", 2)

        val board: Board = emptyBoard.updated(src, player1.marble)

        // verify
        val newGame = createGame(board).move(player2, src, dst)
        newGame.failure.exception must be(a[IllegalMoveException])
      }

      "be forbidden to move a marble to nonempty field" in {
        val src = Pos("a", 1)
        val dst = Pos("a", 2)

        val board: Board = emptyBoard.updated(src, player1.marble).updated(dst, player2.marble)

        // verify
        val newGame = createGame(board).move(player1, src, dst)
        newGame.failure.exception must be(a[IllegalMoveException])
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

        allowedPositions must contain theSameElementsAs List(
          Pos("b", 1), Pos("b", 3),
          Pos("a", 1), Pos("a", 2),
          Pos("c", 2), Pos("c", 3)
        )
      }

    }

    "player wants to move multiple marbles, he should" should {

      "be able to move 2 horizontally" in {
        val pos1 = Pos("b", 1)
        val pos2 = Pos("b", 2)
        val dst = Pos("b", 3)

        val marble = player1.marble

        val board = emptyBoard
          .updated(pos1, marble)
          .updated(pos2, marble)
          .updated(dst, EmptyBoardPiece)

        val newGame: Try[Game] = createGame(board).moveMany(player1, List(pos1, pos2), MoveDirection.East)
        newGame.isSuccess must be(true)
        inside(newGame.success.value) {
          case game: Game =>
            val newBoard: Board = game.board
            newBoard.at(pos1) must equal(EmptyBoardPiece)
            newBoard.at(pos2) must equal(marble)
            newBoard.at(dst) must equal(marble)
        }

      }

      "be able to move 3 marbles horizontally east" in {
        val pos1 = Pos("b1")
        val pos2 = Pos("b2")
        val pos3 = Pos("b3")
        val dst = Pos("b4")

        val marble = player1.marble

        val board = emptyBoard
          .updated(pos1, marble)
          .updated(pos2, marble)
          .updated(pos3, marble)
          .updated(dst, EmptyBoardPiece)

        val newGame: Try[Game] = createGame(board).moveMany(player1, List(pos1, pos2, pos3), MoveDirection.East)
        inside(newGame.success.value) {
          case game: Game =>
            val newBoard: Board = game.board
            newBoard.at(pos1) must equal(EmptyBoardPiece)
            newBoard.at(pos2) must equal(marble)
            newBoard.at(pos3) must equal(marble)
            newBoard.at(dst) must equal(marble)
        }
      }

      "be able to move 3 marbles horizontally west" in {
        val dst = Pos("c1")
        val pos1 = Pos("c2")
        val pos2 = Pos("c3")
        val pos3 = Pos("c4")

        val marble = player1.marble

        val board = emptyBoard
          .updated(dst, EmptyBoardPiece)
          .updated(pos1, marble)
          .updated(pos2, marble)
          .updated(pos3, marble)

        val newGame: Try[Game] = createGame(board).moveMany(player1, List(pos1, pos2, pos3), MoveDirection.West)
        inside(newGame.success.value) {
          case game: Game =>
            val newBoard: Board = game.board
            newBoard.at(dst) must equal(marble)
            newBoard.at(pos1) must equal(marble)
            newBoard.at(pos2) must equal(marble)
            newBoard.at(pos3) must equal(EmptyBoardPiece)
        }
      }

      "be forbidden to move 4 horizontally" in {
        val pos1 = Pos("c1")
        val pos2 = Pos("c2")
        val pos3 = Pos("c3")
        val pos4 = Pos("c4")

        val marble = player1.marble

        val board = emptyBoard
          .updated(pos1, marble)
          .updated(pos2, marble)
          .updated(pos3, marble)
          .updated(pos4, marble)

        val newGame: Try[Game] = createGame(board).moveMany(player1, List(pos1, pos2, pos3, pos4), MoveDirection.East)
        newGame.isFailure must be(true)
      }

      "be forbidden to move horizontally if there is not enough space" in {
      }

      "be allowed to move 2 and 3 marbles sideways" in {

      }

      "be forbidden to move more than 3 marbles sideways" in {

      }

      "be allowed to move more than 2 and 3 marbles diagonally" in {

      }

      "be forbidden to move more than 3 marbles diagonally" in {

      }
    }

    "player wants to push other player's marbles, he should" should {

      "be able to push 1 marble with 2 marbles" in {
      }

      "be able to push 2 marbles with 3 marbles" in {
      }

      "be forbidden to push 1 marble" in {
      }

      "be forbidden to push his marbles" in {
      }

      "be forbidden to push with other player's marbles" in {
      }

      "be forbidden to push if there is not enough empty space" in {
      }

    }

  }
}
