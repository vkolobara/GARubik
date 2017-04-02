package main.scala.ga.operators


class Population(val size: Int) {
  
  var population: Vector[Solution] = Vector.empty
  
  def randomize(solSize: Int, min: Int, max: Int) = {
    (for {
      i <- 1 to size
    } yield new Solution(solSize, min, max)).toVector
  }
  
  def getBest() = population.maxBy(_.fitness)
  
}