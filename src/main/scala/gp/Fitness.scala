package main.scala.gp

import main.scala.RubiksCube

trait Fitness {
	  def evaluate(tree: Tree): Double

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


class BasicGPFitness(val cube: RubiksCube) extends Fitness = {
	def evaluate(tree: Tree) = {
		var moves = 50
		val rubiks = cube.copy;
		while (moves > 0 && !rubiks.isSolved) {
			moves -= 1
			rubiks.rotate(tree.execute)
		}

		fitness = moves + sumSides(cube)
	}


}
