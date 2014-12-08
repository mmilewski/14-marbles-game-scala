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
    posToPiece.get(pos) match {
      case None => throw new IllegalArgumentException(s"Position $pos is invalid")
      case Some(piece) => piece
    }
  }

  def overwrite(pos: Pos, newValue: Piece): Board = {
    this.copy(posToPiece.updated(pos, newValue))
  }
}

class Game(val board: Board) {
  def move(requester: Player, oldPos: Pos, newPos: Pos): Try[Game] = {
    val marbleToMove: Piece = board.at(oldPos)
    if (canMove(requester, marbleToMove)) {
      val msg = s"Player ${requester.name} who owns ${requester.marbleColor} marbles cannot move $marbleToMove"
      Failure(new IllegalArgumentException(msg))
    } else {
      val newBoard: Board = board
        .overwrite(newPos, marbleToMove)
        .overwrite(oldPos, NoPiece)

      Success(new Game(newBoard))
    }
  }

  private def canMove(player: Player, marbleToMove: Piece): Boolean = {
    marbleToMove.isNonEmpty && marbleToMove != player.marble
  }
}
