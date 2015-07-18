package core

import scala.util.{Failure, Success, Try}

case class Pos(row: String, column: Int) {
  require(row.length == 1)

  def moved(direction: MoveDirection): Pos = {
    direction match {
      case MoveDirection.East => Pos(row, column + 1)
      case MoveDirection.West => Pos(row, column - 1)
      case MoveDirection.NorthEast => ???
      case MoveDirection.NorthWest => ???
      case MoveDirection.SouthEast => ???
      case MoveDirection.SouthWest => ???
    }
  }

  def isHorizontalNeighbourOf(other: Pos): Boolean = {
    this.moved(MoveDirection.East) == other
  }
}

object Pos {
  /**
   * Create object from "a1" instead of ("a", 1) - only for convenience
   */
  def apply(position: String): Pos = {
    val s = position.toLowerCase
    require(s.length == 2)
    Pos(s.substring(0, 1), s.substring(1, 2).toInt)
  }
}

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

  /**
   * @param ps Positions must be specified from left to right, if horizontal. Diagonal positions are not supported yet.
   */
  def moveMany(requester: Player, ps: Seq[Pos], direction: MoveDirection): Try[Game] = {
    ps match {
      case Seq() =>
        Success(this)
      case Seq(pos) =>
        move(requester, pos, pos.moved(direction))
      case seq if seq.length <= 3 =>
        require(Set[MoveDirection](MoveDirection.East, MoveDirection.West).contains(direction))
        val allHorizontalNeighbours = (seq zip (seq.tail)).map { case (p, r) => p.isHorizontalNeighbourOf(r) }.reduce(_ && _)
        allHorizontalNeighbours match {
          case true =>
            val newBoard = if (direction == MoveDirection.East) {
              board
                .updated(seq.head, EmptyBoardPiece)
                .updated(seq.last.moved(direction), requester.marble)
            } else if (direction == MoveDirection.West) {
              board
                .updated(seq.head.moved(direction), requester.marble)
                .updated(seq.last, EmptyBoardPiece)
            } else {
              ???
            }

            Success(new Game(newBoard))
          case false =>
            Failure(new IllegalMoveException(s"Cannot move $seq towards $direction"))
        }
      case seq =>
        Failure(new IllegalMoveException(s"Cannot move $seq marbles towards $direction because there are more than 3"))
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
