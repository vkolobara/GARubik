import Array._

class RubiksCube {

  var front = new Face(0)
  var back = new Face(1)

  var top = new Face(2)
  var bottom = new Face(3)

  var left = new Face(4)
  var right = new Face(5)

  var sides = front :: back :: top :: bottom :: left :: right :: Nil

  def isSolved(): Boolean = {
    def isSolved(sides: List[Face]): Boolean = {

      if (sides.isEmpty) true
      else sides.head.isSolved && isSolved(sides.tail)

    }
    isSolved(sides)
  }
  
  def copy(): RubiksCube = {
    var newRubik = new RubiksCube
    
    newRubik.front = this.front.copy
    newRubik.back = this.back.copy
    newRubik.top = this.top.copy
    newRubik.bottom = this.bottom.copy
    newRubik.left = this.left.copy
    newRubik.right = this.right.copy
    
    newRubik.sides = newRubik.front ::  newRubik.back ::  newRubik.top ::  newRubik.bottom ::  newRubik.left ::  newRubik.right :: Nil
    
    newRubik
    
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

    left.setColumn(2, rotated(0))
    right.setColumn(0, rotated(1))

    top.setRow(2, rotated(2))
    bottom.setRow(0, rotated(3))

  }

  def rotateBack(clockwise: Boolean) = {
    back.rotate(clockwise)

    var leftCol = right.getColumn(2)
    var rightCol = left.getColumn(0)
    var topRow = top.getRow(0).reverse
    var bottomRow = bottom.getRow(2).reverse

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    right.setColumn(2, rotated(0))
    left.setColumn(0, rotated(1))
    top.setRow(0, rotated(2).reverse)
    bottom.setRow(2, rotated(3).reverse)

  }

  def rotateLeft(clockwise: Boolean) = {
    left.rotate(clockwise)

    var leftCol = back.getColumn(2)
    var rightCol = front.getColumn(0)
    var topRow = top.getColumn(0)
    var bottomRow = bottom.getColumn(0).reverse

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    back.setColumn(2, rotated(0))
    front.setColumn(0, rotated(1))

    top.setColumn(0, rotated(2))
    bottom.setColumn(0, rotated(3).reverse)
  }

  def rotateRight(clockwise: Boolean) = {
    right.rotate(clockwise)

    var leftCol = front.getColumn(2)
    var rightCol = back.getColumn(0)
    var topRow = top.getColumn(2).reverse
    var bottomRow = bottom.getColumn(2)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    front.setColumn(2, rotated(0))
    back.setColumn(0, rotated(1))
    top.setColumn(2, rotated(2).reverse)
    bottom.setColumn(2, rotated(3))
  }

  def rotateTop(clockwise: Boolean) = {
    top.rotate(clockwise)

    var leftCol = left.getRow(0)
    var rightCol = right.getRow(0).reverse
    var topRow = back.getRow(0).reverse
    var bottomRow = front.getRow(0)

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    left.setRow(0, rotated(0))
    right.setRow(0, rotated(1).reverse)
    back.setRow(0, rotated(2).reverse)
    front.setRow(0, rotated(3))

  }

  def rotateBottom(clockwise: Boolean) = {
    bottom.rotate(clockwise)

    var leftCol = left.getRow(2).reverse
    var rightCol = right.getRow(2)
    var topRow = front.getRow(2)
    var bottomRow = back.getRow(2).reverse

    val rotated = rotateOther(clockwise, leftCol, rightCol, topRow, bottomRow)

    left.setRow(2, rotated(0).reverse)
    right.setRow(2, rotated(1))
    front.setRow(2, rotated(2))
    back.setRow(2, rotated(3).reverse)
  }

  def rotateSide(operation: Int): Unit = {
    rotateSide(operation / 2, operation % 2 == 0)
  }
  
  def rotateSide(side: Int, clockwise: Boolean) = {
    side match {
      case 0 => rotateTop(clockwise)
      case 1 => rotateBottom(clockwise)
      case 2 => rotateLeft(clockwise)
      case 3 => rotateRight(clockwise)
      case 4 => rotateFront(clockwise)
      case 5 => rotateBack(clockwise)
    }
  }

  def scramble(numRotations: Int) = {
    val rand = scala.util.Random
    for (i <- 1 to numRotations) {
      val side = rand.nextInt(6)
        rotateSide(side, rand.nextBoolean) 
    }
  }

  override def toString() = {
    def sideToString(sides: List[Face]): String = {
      if (sides.isEmpty) ""
      else sides.head.toString() + "\n\n" + sideToString(sides.tail)
    }

    sideToString(sides) + "\n"
  }

}

