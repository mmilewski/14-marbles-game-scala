package core.v2

//
case class Pos(position: String)

//
case class MarbleColor(color: String)

object MarbleColor {
  val WHITE = MarbleColor("white")
  val BLACK = MarbleColor("black")
}

//
sealed trait Marble

case class PositionedMarble(owner: Player, pos: Pos) extends Marble

case class DeadMarble(owner: Player) extends Marble

//
case class Player(name: String, marbleColor: MarbleColor)

//
sealed trait GameState

case class GameInProgress(players: List[Player], playerOnMove: Player) extends GameState

case class GameFinished(winner: Player) extends GameState

//
sealed trait MoveDirection

object MoveDirection {

  case object West extends MoveDirection

  case object East extends MoveDirection

  case object NorthWest extends MoveDirection

  case object NorthEast extends MoveDirection

  case object SouthWest extends MoveDirection

  case object SouthEast extends MoveDirection

}

//
sealed trait MoveCmd

case class MoveSingle(marble: PositionedMarble, direction: MoveDirection) extends MoveCmd

//
sealed trait AttackCmd

case class AttackWith2(m1: PositionedMarble, m2: PositionedMarble, direction: MoveDirection) extends AttackCmd

case class AttackWith3(m1: PositionedMarble, m2: PositionedMarble, m3: PositionedMarble, direction: MoveDirection) extends AttackCmd

//
case class Board(ignore: Unit)

class GameController {
  def move(game: GameInProgress, cmd: MoveCmd): GameInProgress = {
    cmd match {
      case MoveSingle(marble, direction) =>
    }

    game
  }

  //  def attack(game: GameInProgress , cmd: AttackCmd): GameState = {
  //  }
  //

}

object Bootstrap {

  def play(p1: Player, p2: Player, board: Board): GameController = {

    new GameController
  }

  private val player1: Player = Player("p1", MarbleColor("white"))
  private val player2: Player = Player("p2", MarbleColor("black"))
  private val board = Board(())
  val gameCtrl = play(player1, player2, board)

  val marble = PositionedMarble(player1, Pos("c2"))

  import MoveDirection._

  {

    val game = GameInProgress(List(player1, player2), playerOnMove = player1)
    val commands = List(
      MoveSingle(marble, West)
    )

  }
}


