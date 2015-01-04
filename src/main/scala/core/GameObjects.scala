package core

import scala.util.{Failure, Success, Try}

case class Pos(row: String, column: Int)

case class Player(name: String, marbleColor: String) {
  val marble = Marble(marbleColor)
}

trait Piece {
  def isEmpty: Boolean = false

  def isNonEmpty = !isEmpty
}

case object NoPiece extends Piece {
  override def isEmpty: Boolean = true
}

case class Marble(color: String) extends Piece

case class Board(posToPiece: Map[Pos, Piece]) {
  def at(pos: Pos): Piece = {
    atOption(pos) match {
      case None => throw new IllegalArgumentException(s"Position $pos is invalid")
      case Some(piece) => piece
    }
  }

  def atOption(pos: Pos): Option[Piece] = posToPiece.get(pos)

  def exist(pos: Pos): Boolean = posToPiece.isDefinedAt(pos)

  def overwrite(pos: Pos, newValue: Piece): Board = {
    this.copy(posToPiece.updated(pos, newValue))
  }
}

case class IllegalMoveException(msg: String) extends RuntimeException

class Game(val board: Board) {
  def move(requester: Player, srcPost: Pos, dstPost: Pos): Try[Game] = {

    checkThisMove(board, requester, srcPost, dstPost) match {
      case Success(_) =>
        val marbleToMove: Piece = board.at(srcPost)
        val newBoard: Board =
          board
            .overwrite(dstPost, marbleToMove)
            .overwrite(srcPost, NoPiece)
        Success(new Game(newBoard))

      case Failure(ex) => Failure(ex)
    }

  }

  private def checkThisMove(board: Board, player: Player, srcPost: Pos, dstPost: Pos): Try[Unit] = {
    val checks = Stream(
      (() => board.exist(srcPost) && board.exist(dstPost),
        IllegalMoveException(s"One of the positions is outside of the board, the move was $srcPost -> $dstPost")),
      (() => areNeighbours(srcPost, dstPost),
        IllegalMoveException(s"Cannot move between fields which are not neighbours $srcPost -> $dstPost")),
      (() => board.atOption(srcPost).contains(player.marble),
        IllegalMoveException(s"Player can move only his marbles (i.e. ${player.marble}"))
    )

    checks.dropWhile(_._1()).headOption match {
      case Some((_, ex)) => Failure(ex)
      case None => Success(())
    }

  }

  /**
   * Positions passed as arguments must be valid (i.e. exist on board)
   */
  private def areNeighbours(srcPost: Pos, dstPost: Pos): Boolean = {
    def rowToInt(a: String): Int = a.charAt(0).asDigit

    def upperRow(a: String, b: String): Boolean = rowToInt(a) + 1 == rowToInt(b)
    def lowerRow(a: String, b: String): Boolean = rowToInt(a) - 1 == rowToInt(b)
    def rightColumn(a: Int, b: Int): Boolean = a + 1 == b
    def leftColumn(a: Int, b: Int): Boolean = a - 1 == b

    (srcPost, dstPost) match {
      case (Pos(oldRow, oldCol), Pos(newRow, newCol)) =>
        ((oldCol == newCol && (upperRow(oldRow, newRow) || lowerRow(oldRow, newRow)))
          || (leftColumn(oldCol, newCol) && ((oldRow == newRow) || lowerRow(oldRow, newRow)))
          || (rightColumn(oldCol, newCol) && ((oldRow == newRow) || upperRow(oldRow, newRow))))
    }
  }
}
