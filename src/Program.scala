import MarsChars._

object Program extends App {
  implicit val world = World(5,5)

  val rover = Rover(Point(1, 2), 'N'.toOrientation)
  val result = deployRover(rover, "LMLMLMLMM")
  verifyRoverOutput(result, Point(1, 3), North)
  
  val rover2 = Rover(Point(3, 3), 'E'.toOrientation)
  val result2 = deployRover(rover2, "MMRMMRMRRM")
  verifyRoverOutput(result2, Point(5, 1), East)

  def deployRover(rover: Rover, commands: String) = {
    commandsFromString(commands).foldLeft(rover)(_.processCommand(_))
  }

  def verifyRoverOutput(result: Rover, endPosition: Point, endOrientation: Orientation) {
    println(result)
    assert(endPosition == result.position)
    assert(endOrientation == result.orientation)
  }
  
  def commandsFromString(commands: String) = {
    commands map {
      case Left.letter => Left
      case Right.letter => Right
      case Move.letter => Move
    } 
  }
}