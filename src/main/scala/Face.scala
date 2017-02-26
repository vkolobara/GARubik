package main.scala

class Face(color: Int) {
  var face = Array.fill(3, 3)(color)

  def elem(i: Int)(j: Int) = face(i)(j)

  def getRow(index: Int): Array[Int] = {
    face(index)
  }

  def getColumn(index: Int): Array[Int] = {
    face.map { _(index) }
  }

  def setRow(index: Int, newRow: Array[Int]) = {
    face(index) = newRow;
  }

  def setColumn(index: Int, newColumn: Array[Int]) = {
    for (i <- 0 to 2) face(i)(index) = newColumn(i)
  }

  def isSolved(): Boolean = {
    val elem = face(0)(0)
    for {
      row <- face
      el <- row
    } if (el != elem) return false

    return true
  }
  
  def copy(): Face = {
    var newFace = new Face(0)
    newFace.face = face.map(_.clone)
    newFace
  }

  def rotate(clockwise: Boolean) {

    var firstRowIndex = 0
    var lastRowIndex = 2

    var firstCol = getColumn(0).clone.reverse
    var lastCol = getColumn(2).clone

    var firstRow = getRow(firstRowIndex).clone
    var lastRow = getRow(lastRowIndex).clone

    if (!clockwise) {
      firstRowIndex = 2
      lastRowIndex = 0

      firstRow = getRow(firstRowIndex).clone.reverse
      lastRow = getRow(lastRowIndex).clone.reverse
      
      lastCol = lastCol.reverse
    }

    setRow(firstRowIndex, firstCol)
    setColumn(2, firstRow)
    setRow(lastRowIndex, lastCol.reverse)
    setColumn(0, lastRow)

  }

  override def toString() = {
    face.map(_.mkString).mkString("\n")
  }

}