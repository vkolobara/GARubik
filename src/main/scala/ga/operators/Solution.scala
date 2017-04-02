package main.scala.ga.operators

import scala.util.Random

class Solution(val size: Int, val min: Int, val max: Int) {

  private val rand: Random = new Random
  var sol: Vector[Int] = randomize()
  var fitness: Double = 0
  var bestIndex: Int = 0

  def randomize() = {
    (for {
      i <- 1 to size
    } yield rand.nextInt(max - min) + min).toVector
  }
  
  def setFitness(fit: Double): Solution = {
    this.fitness = fit
    this
  }


  def duplicate() = {
    var sol = new Solution(size, min, max)
    sol.sol = this.sol.map(x => x)
    sol.fitness = this.fitness
    sol
  }
}