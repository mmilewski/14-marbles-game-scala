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

  "When a game is started, the player" should {
    val oldPos = Pos("a", 1)
    val newPos = Pos("a", 2)

    "be forbidden to move a marble he does not own" in {
      val board: Board = emptyBoard.overwrite(oldPos, player1.marble)
      board.at(oldPos) mustNotEqual NoPiece

      // verify
      createGame(board).move(player2, oldPos, newPos) must beFailedTry.withThrowable[IllegalArgumentException]
    }

    "be able to move a marble which belongs to him" in {
      val board: Board = emptyBoard.overwrite(oldPos, player1.marble)
      board.at(oldPos) mustEqual player1.marble

      // verify
      createGame(board).move(player1, oldPos, newPos) must beSuccessfulTry.like {
        case game: Game =>
          val newBoard: Board = game.board
          newBoard.at(oldPos) mustEqual NoPiece
          newBoard.at(newPos) mustEqual player1.marble
          success
      }
    }

  }
}
