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

case class Rover(val position: Point, val orientation: Orientation)(implicit world: World) {
  def processCommand(command: Command) = command match {
    case Left => this.copy(orientation = orientation.left)
    case Right => this.copy(orientation = orientation.right)
    case Move => this.copy(position = world.moveInWorld(position, orientation))
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
