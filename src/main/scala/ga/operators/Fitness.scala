package main.scala.ga.operators

import main.scala.RubiksCube
import scala.util.control.Breaks._;

trait Fitness {
  def evaluate(sol: Solution): Double
  
  def sumSides(cube: RubiksCube): Double = {
    var sum = 0.0;

    for (side <- cube.sides) {
      val center = side.elem(1)(1)
      for (row <- 0 to 2) {
        for (col <- 0 to 2)
          if (side.face(row)(col) == center && (row != 1 || col != 1)) sum += 1
      }

    }

    sum
  }
}

class MaxCubiesFitness(val rubiksCube: RubiksCube) extends Fitness {

  def evaluate(sol: Solution): Double = {

    var rubiks = rubiksCube.copy

    var fitness = 0.0;
    var index = 0;

    breakable {
      for (gene <- sol.sol) {
        val newFit = evaluateCube(rubiks) + sumSides(rubiks)
        if (newFit > fitness) {
          fitness = newFit
          index = sol.sol.indexOf(gene)
        }
        if (rubiks.isSolved) break
        rubiks.rotateSide(gene)
      }
    }

    sol.bestIndex = index
    fitness

  }

  def evaluateCube(cube: RubiksCube): Double = {
    var sum = 0.0

    var tmpSum = 0.0
    for (i <- 0 to 11) {
      if (cube.getEdgeCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum += 4 * tmpSum;

    tmpSum = 0.0;

    for (i <- 0 to 7) {
      if (cube.getCornerCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum + 6 * tmpSum
  }
}

class CubiesFitness(val rubiksCube: RubiksCube) extends Fitness {

  def evaluate(sol: Solution): Double = {

    var rubiks = rubiksCube.copy

    var fitness = 0.0;

    breakable {
      for (gene <- sol.sol) {
        rubiks.rotateSide(gene)
        fitness += evaluateCube(rubiks) + sumSides(rubiks)
        if (rubiks.isSolved) break;
      }
    }

    fitness / 40.0;

  }

  def evaluateCube(cube: RubiksCube): Double = {
    var sum = 0.0

    var tmpSum = 0.0
    for (i <- 0 to 11) {
      if (cube.getEdgeCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum += 4 * tmpSum;

    tmpSum = 0.0;

    for (i <- 0 to 7) {
      if (cube.getCornerCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum + 6 * tmpSum
  }
}

class SideCompleteFitness(val rubiksCube: RubiksCube) extends Fitness {

  private val maxFit = 40.0;

  def evaluate(sol: Solution): Double = {
    var fitness = maxFit;
    var rubiks = rubiksCube.copy

    breakable {
      for (gene <- sol.sol) {
        rubiks.rotateSide(gene)
        fitness = fitness - 1
        if (rubiks.isSolved) break;
      }
    }

    fitness + sumSides(rubiks);
  }

}