package main.scala

import Array._
import main.scala.ga.operators.Solution

class RubiksCube {

  final val FRONT = 'F'
  final val BACK = 'B'
  final val UP = 'U'
  final val DOWN = 'D'
  final val LEFT = 'L'
  final val RIGHT = 'R'

  final val SIDES = 'F' :: 'B' :: 'U' :: 'D' :: 'L' :: 'R' :: Nil

  var front = new Face(FRONT)
  var back = new Face(BACK)

  var top = new Face(UP)
  var bottom = new Face(DOWN)

  var left = new Face(LEFT)
  var right = new Face(RIGHT)

  var sides = front :: back :: top :: bottom :: left :: right :: Nil

  def getCornerCubie(index: Int): CornerCubie = {
    index match {
      case 0 => new CornerCubie((2, top.elem(2)(0)), (0, front.elem(0)(0)), (4, left.elem(0)(2)))
      case 1 => new CornerCubie((2, top.elem(2)(2)), (0, front.elem(0)(2)), (5, right.elem(0)(0)))

      case 2 => new CornerCubie((3, bottom.elem(0)(0)), (0, front.elem(2)(0)), (4, left.elem(2)(2)))
      case 3 => new CornerCubie((3, bottom.elem(0)(2)), (0, front.elem(2)(2)), (5, right.elem(2)(0)))

      case 4 => new CornerCubie((2, top.elem(0)(2)), (1, back.elem(0)(0)), (5, right.elem(0)(2)))
      case 5 => new CornerCubie((2, top.elem(0)(0)), (1, back.elem(0)(2)), (4, left.elem(0)(0)))

      case 6 => new CornerCubie((3, bottom.elem(2)(0)), (1, back.elem(2)(2)), (4, left.elem(2)(0)))
      case 7 => new CornerCubie((3, bottom.elem(2)(2)), (1, back.elem(2)(0)), (5, right.elem(2)(2)))
    }
  }

  def getEdgeCubie(index: Int): EdgeCubie = {
    index match {
      case 0  => new EdgeCubie((0, front.elem(1)(0)), (4, left.elem(1)(2)))
      case 1  => new EdgeCubie((0, front.elem(1)(2)), (5, right.elem(1)(0)))
      case 2  => new EdgeCubie((0, front.elem(0)(1)), (2, top.elem(2)(1)))
      case 3  => new EdgeCubie((0, front.elem(2)(1)), (3, bottom.elem(0)(1)))

      case 4  => new EdgeCubie((1, back.elem(1)(0)), (5, right.elem(1)(2)))
      case 5  => new EdgeCubie((1, back.elem(1)(2)), (4, left.elem(1)(0)))
      case 6  => new EdgeCubie((1, back.elem(0)(1)), (2, top.elem(0)(1)))
      case 7  => new EdgeCubie((1, back.elem(2)(1)), (3, bottom.elem(2)(1)))

      case 8  => new EdgeCubie((5, right.elem(0)(1)), (2, top.elem(1)(2)))
      case 9  => new EdgeCubie((5, right.elem(2)(1)), (3, bottom.elem(1)(2)))

      case 10 => new EdgeCubie((4, left.elem(0)(1)), (2, top.elem(1)(0)))
      case 11 => new EdgeCubie((4, left.elem(2)(1)), (3, bottom.elem(1)(0)))
    }

  }

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

    newRubik.sides = newRubik.front :: newRubik.back :: newRubik.top :: newRubik.bottom :: newRubik.left :: newRubik.right :: Nil

    newRubik

  }

  /**
   * Faces appear in order: left, right, top, bottom (looking from current face to rotate)
   */
  private def rotateOther(clockwise: Boolean, left: Array[Char], right: Array[Char], top: Array[Char], bottom: Array[Char]): List[Array[Char]] = {

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
    if (operation < 12) rotateSide(operation / 2, operation % 2 == 0)
    else rotateSideComp(operation % 12)
  }

  def decode(genes: Vector[Int], index: Int): String = {

    def decodeSimple(gene: Int): String = {
      var symbol = "'"
      if (gene % 2 == 0) symbol = ""

      SIDES(gene / 2).toString + symbol
    }

    def decodeComplex(gene: Int): String = {
      SIDES(gene % 12).toString + "2"
    }

    if (genes.isEmpty || index == 0) ""
    else if (genes.head < 12) decodeSimple(genes.head) + " " + decode(genes.tail, index - 1)
    else decodeComplex(genes.head) + " " + decode(genes.tail, index - 1)

  }

  def rotateSideComp(side: Int) = {
    side match {
      case 0 => rotateFront(true); rotateFront(true);
      case 1 =>
        rotateBack(true); rotateBack(true)
      case 2 =>
        rotateTop(true); rotateTop(true)
      case 3 =>
        rotateBottom(true); rotateBottom(true)
      case 4 =>
        rotateLeft(true); rotateLeft(true)
      case 5 => rotateRight(true); rotateRight(true)
    }
  }
  def rotateSide(side: Int, clockwise: Boolean) = {
    side match {
      case 0 => rotateFront(clockwise)
      case 1 => rotateBack(clockwise)
      case 2 => rotateTop(clockwise)
      case 3 => rotateBottom(clockwise)
      case 4 => rotateLeft(clockwise)
      case 5 => rotateRight(clockwise)
    }
  }

  def scramble(numRotations: Int) = {
    val rand = scala.util.Random
    var ret: Vector[Int] = Vector.empty
    
    for (i <- 1 to numRotations) {
      val side = rand.nextInt(6)
      val clockwise = rand.nextBoolean
      rotateSide(side, clockwise)
      var value = side * 2;
      if (!clockwise) value += 1
      ret = ret :+ value
    }

    ret

  }

  def printCubeWithSolution(sol: Solution) = {
    var cube = this.copy
    println(sol.bestIndex)
    for (i <- 0 until sol.bestIndex) {
      cube.rotateSide(sol.sol(i))
    }
    println(cube)
  }

  override def toString() = {
    def sideToString(sides: List[Face]): String = {
      if (sides.isEmpty) ""
      else sides.head.face(1)(1) + "\n" + sides.head.toString() + "\n\n" + sideToString(sides.tail)
    }

    sideToString(sides) + "\n"
  }

}

