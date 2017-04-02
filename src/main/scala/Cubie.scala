package main.scala

// Each face is (index, color)
class CornerCubie(val topFace: (Int, Char), val frontFace: (Int, Char), val sideFace: (Int, Char)) {
  def isCorrect(rubiksCube: RubiksCube): Boolean = {
    rubiksCube.sides(topFace._1).elem(1)(1) == topFace._2 &&
    rubiksCube.sides(frontFace._1).elem(1)(1) == frontFace._2 &&
    rubiksCube.sides(sideFace._1).elem(1)(1) == sideFace._2 
  }
}

// Each face is (index, color)
class EdgeCubie(val leftFace: (Int, Char), val rightFace: (Int, Char)) {
  def isCorrect(rubiksCube: RubiksCube): Boolean = {
    rubiksCube.sides(leftFace._1).elem(1)(1) == leftFace._2 &&
    rubiksCube.sides(rightFace._1).elem(1)(1) == rightFace._2
  }
}