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

sealed abstract class Command(val letter: Char)
case object Move extends Command('M')
case object Left extends Command('L')
case object Right extends Command('R')

object Orientation {
  private val ORDER = Map(North.ordinal -> North,
    East.ordinal -> East,
    South.ordinal -> South,
    West.ordinal -> West)
}
sealed abstract class Orientation(val letter: Char, val ordinal: Int) {
  def right = move(_ + 1)
  def left = move(_ - 1)
  def move(func: Int => Int) = {
	val newIndex = (Orientation.ORDER.size + func(this.ordinal)) % Orientation.ORDER.size
	Orientation.ORDER(newIndex)
  }
}
case object North extends Orientation('N', 0)
case object West extends Orientation('W', 3)
case object South extends Orientation('S', 2)
case object East extends Orientation('E', 1)
case class Point(x: Int, y: Int)

case class Rover(var position: Point, var orientation: Orientation, world: World) {
  def processCommand(command: Command) = command match {
    case Left => orientation = orientation.left
    case Right => orientation = orientation.right 
    case Move => position = world.moveInWorld(position, orientation)
  }
  
  override def toString() = {
    s"${position.x} ${position.y} $orientation"
  }
}

case class World(topX: Int, topY: Int) {
  val bottomX = 0
  val bottomY = 0

  def moveInWorld(position: Point, orientation: Orientation): Point = {
    val Point(x, y) = position
    orientation match {
      case North => position.copy(y = if (y < topY) inc(y) else y)
      case South => position.copy(y = if (y > bottomY) dec(y) else y)
      case East => position.copy(x = if (x < topX) inc(x) else x)
      case West => position.copy(x = if (x > bottomX) dec(x) else x)
    }
  }

  def inc(i: Int) = i + 1
  def dec(i: Int) = i - 1
}

object Program extends App {
  import MarsChars._
  val world = World(5,5)

  val rover = Rover(Point(1, 2), 'N'.toOrientation, world)
  commandsFromString("LMLMLMLMM") map rover.processCommand
  assert(Point(1,3) == rover.position)
  assert(North == rover.orientation)
  
  val rover2 = Rover(Point(3, 3), 'E'.toOrientation, world)
  commandsFromString("MMRMMRMRRM") map rover2.processCommand
  assert(Point(5,1) == rover2.position)
  assert(East == rover2.orientation)
  
  def commandsFromString(commands: String) = {
    commands map {
      case Left.letter => Left
      case Right.letter => Right
      case Move.letter => Move
    } 
  }
}