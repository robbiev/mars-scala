object MarsChars {
  implicit def toMarsChar(char: Char) = MarsChar(char)
  case class MarsChar(char: Char) {
    def toOrientation = char match {
      case North.letter => North
      case East.letter => East
      case South.letter => South
      case West.letter => West
    }
  }
}

object Program extends App {
  import MarsChars._
  val world = World(5,5)
  val rover = Rover(Point(1, 2), 'N'.toOrientation, world)
  commandsFromString("LMLMLMLMM") map rover.processCommand
  println(rover)
  
  val rover2 = Rover(Point(3, 3), 'E'.toOrientation, world)
  commandsFromString("MMRMMRMRRM") map rover2.processCommand
  println(rover2)
  /* 
    1 3 N
    5 1 E
   */
  
  def commandsFromString(commands: String) = {
    commands map {
      case Left.letter => Left
      case Right.letter => Right
      case Move.letter => Move
    } 
  }
}

sealed trait Command {
  val letter: Char
}
case object Move extends Command {
  val letter = 'M'
}
case object Left extends Command {
  val letter = 'L'
}
case object Right extends Command {
  val letter = 'R'
}
object Orientation {
  val ORDER = North :: East ::South :: West :: Nil
}
sealed trait Orientation {
  val letter: Char
}
case object North extends Orientation {
  val letter = 'N'
}
case object West extends Orientation {
  val letter = 'W'
}
case object South extends Orientation {
  val letter = 'S'
}
case object East extends Orientation {
  val letter = 'E'
}
case class Point(x: Int, y: Int)
case class Rover(var position: Point, var direction: Orientation, world: World) {
  
  def mutateDirection(func: Int => Int): Unit = {
	val newIndex = (Orientation.ORDER.length + func(Orientation.ORDER.indexOf(direction))) % Orientation.ORDER.length
	direction = Orientation.ORDER(newIndex)
  }
  
  def move(): Unit = {
    position = world.moveInWorld(position, direction)
  }
  
  def processCommand(command: Command) = command match {
    case Left => mutateDirection(world.dec)
    case Right => mutateDirection(world.inc)
    case Move => move()
  }
  
  override def toString() = {
    s"${position.x} ${position.y} $direction"
  }
}

case class World(topX: Int, topY: Int) {
  val bottomX = 0
  val bottomY = 0

  def moveInWorld(position: Point, direction: Orientation): Point = {
    val Point(x, y) = position
    direction match {
      case North => position.copy(y = if (y < topY) inc(y) else y)
      case South => position.copy(y = if (y > bottomY) dec(y) else y)
      case East => position.copy(x = if (x < topX) inc(x) else x)
      case West => position.copy(x = if (x > bottomX) dec(x) else x)
    }
  }

  def inc(i: Int) = i + 1
  def dec(i: Int) = i - 1
}