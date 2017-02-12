import Array._

class RubiksCube {

  private var front = new Face(0)
  private var back = new Face(1)

  private var top = new Face(2)
  private var bottom = new Face(3)

  private var left = new Face(4)
  private var right = new Face(5)

  private var sides = front :: back :: top :: bottom :: left :: right :: Nil

  def isSolved(): Boolean = {
    def isSolved(sides: List[Face]): Boolean = {

      if (sides.isEmpty) true
      else sides.head.isSolved && isSolved(sides.tail)

    }
    isSolved(sides)
  }

  /**
   * Faces appear in order: left, right, top, bottom (looking from current face to rotate)
   */
  private def rotateOther(clockwise: Boolean, left: Array[Int], right: Array[Int], top: Array[Int], bottom: Array[Int]): List[Array[Int]] = {

    var leftCol = left.clone.reverse
    var rightCol = right.clone

    var topRow = top.clone
    var botRow = bottom.clone

    if (!clockwise) {

      var tmp = topRow.reverse;

      topRow = botRow.reverse
      botRow = tmp

      tmp = leftCol
      leftCol = rightCol.reverse
      rightCol = tmp
    }

    botRow :: topRow :: leftCol :: rightCol.reverse :: Nil

  }

  def rotateFront(clockwise: Boolean) = {
    front.rotate(clockwise)

    var leftCol = left.getColumn(2)
    var rightCol = right.getColumn(0)
    var topRow = top.getRow(2)
    var bottomRow = bottom.getRow(0)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)

  }

  def rotateBack(clockwise: Boolean) = {
    back.rotate(clockwise)

    var leftCol = right.getColumn(2)
    var rightCol = left.getColumn(0)
    var topRow = top.getRow(0).reverse
    var bottomRow = bottom.getRow(2).reverse

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)
  }

  def rotateLeft(clockwise: Boolean) = {
    left.rotate(clockwise)

    var leftCol = back.getColumn(2)
    var rightCol = front.getColumn(0)
    var topRow = top.getColumn(0)
    var bottomRow = bottom.getColumn(0).reverse

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)
  }

  def rotateRight(clockwise: Boolean) = {
    right.rotate(clockwise)

    var leftCol = front.getColumn(2)
    var rightCol = back.getColumn(0)
    var topRow = top.getColumn(2).reverse
    var bottomRow = bottom.getColumn(2)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)
  }

  def rotateTop(clockwise: Boolean) = {
    top.rotate(clockwise)

    var leftCol = left.getColumn(2)
    var rightCol = right.getColumn(0)
    var topRow = top.getRow(2)
    var bottomRow = bottom.getRow(0)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)
  }

  def rotateBottom(clockwise: Boolean) = {
    bottom.rotate(clockwise)

    var leftCol = left.getColumn(2)
    var rightCol = right.getColumn(0)
    var topRow = top.getRow(2)
    var bottomRow = bottom.getRow(0)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    leftCol = rotated(0)
    rightCol = rotated(1)
    topRow = rotated(2)
    bottomRow = rotated(3)
  }
}

