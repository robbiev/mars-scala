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