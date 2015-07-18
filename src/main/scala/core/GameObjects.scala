package core

import scala.util.{Failure, Success, Try}

case class Pos(row: String, column: Int)

case class Player(name: String, marbleColor: String) {
  val marble = Marble(marbleColor)
}

trait BoardPiece

case object EmptyBoardPiece extends BoardPiece

case class Marble(color: String) extends BoardPiece

case class Board(posToPiece: Map[Pos, BoardPiece]) {
  def at(pos: Pos): BoardPiece = {
    atOption(pos) match {
      case None => throw new IllegalArgumentException(s"Position $pos is invalid")
      case Some(piece) => piece
    }
  }

  def atOption(pos: Pos): Option[BoardPiece] = posToPiece.get(pos)

  def exist(pos: Pos): Boolean = posToPiece.isDefinedAt(pos)

  def updated(pos: Pos, newValue: BoardPiece): Board = {
    this.copy(posToPiece = posToPiece.updated(pos, newValue))
  }
}

sealed trait MoveDirection

object MoveDirection {

  case object West extends MoveDirection

  case object East extends MoveDirection

  case object NorthWest extends MoveDirection

  case object NorthEast extends MoveDirection

  case object SouthWest extends MoveDirection

  case object SouthEast extends MoveDirection

}

case class IllegalMoveException(msg: String) extends RuntimeException

class Game(val board: Board) {
  def move(requester: Player, src: Pos, dst: Pos): Try[Game] = {

    checkThisMove(board, requester, src, dst) match {
      case Success(_) =>
        val marbleToMove: BoardPiece = board.at(src)
        val newBoard: Board =
          board
            .updated(dst, marbleToMove)
            .updated(src, EmptyBoardPiece)
        Success(new Game(newBoard))

      case Failure(ex) => Failure(ex)
    }

  }

  def moveMany(requester: Player, ps: Seq[Pos], direction: MoveDirection): Try[Game] = {
    ps match {
      case Seq(p1, p2) =>
        val newBoard =
          board
            .updated(p1, EmptyBoardPiece)
            .updated(p2, requester.marble)
            .updated(Pos("b", 3), requester.marble)
        Success(new Game(newBoard))
      case _ => Failure(new IllegalStateException(s"Couldn't handle move: $direction, $ps"))
    }

  }

  private def checkThisMove(board: Board, player: Player, src: Pos, dst: Pos): Try[Unit] = {
    val checks: Seq[(() => Boolean, IllegalMoveException)] = Seq(
      (() => board.exist(src) && board.exist(dst),
        IllegalMoveException(s"One of the positions is outside of the board, the move was: $src -> $dst")),
      (() => areNeighbours(src, dst),
        IllegalMoveException(s"Cannot move between fields which are not neighbours $src -> $dst")),
      (() => board.atOption(src).contains(player.marble),
        IllegalMoveException(s"Player can move only his marbles (i.e. ${player.marble}). The move was: $src -> $dst")),
      (() => board.at(dst) == EmptyBoardPiece,
        IllegalMoveException(s"Destination field must be empty to move a marble there. The move was: $src -> $dst"))
    )

    val firstViolation = checks.find(_._1() == false)
    firstViolation match {
      case Some((_, ex)) => Failure(ex)
      case None => Success(())
    }

  }

  /**
   * Positions passed as arguments must be valid (i.e. exist on board)
   */
  private def areNeighbours(src: Pos, dst: Pos): Boolean = {
    def rowToInt(a: String): Int = a.charAt(0).asDigit

    def upperRow(a: String, b: String): Boolean = rowToInt(a) + 1 == rowToInt(b)
    def lowerRow(a: String, b: String): Boolean = rowToInt(a) - 1 == rowToInt(b)
    def rightColumn(a: Int, b: Int): Boolean = a + 1 == b
    def leftColumn(a: Int, b: Int): Boolean = a - 1 == b

    (src, dst) match {
      case (Pos(oldRow, oldCol), Pos(newRow, newCol)) =>
        ((oldCol == newCol && (upperRow(oldRow, newRow) || lowerRow(oldRow, newRow)))
          || (leftColumn(oldCol, newCol) && ((oldRow == newRow) || lowerRow(oldRow, newRow)))
          || (rightColumn(oldCol, newCol) && ((oldRow == newRow) || upperRow(oldRow, newRow))))
    }
  }
}
