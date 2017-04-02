package main.scala.ga.operators

class SteadyStateAlgorithm(crossover: Crossover, mutation: Mutation, selection: Selection, maxIter: Int) {
  
  
  
  def run(fitness: Fitness, popSize: Int, solSize: Int, min: Int, max: Int): Solution = {
    var population = new Population(popSize)
    population.population = population.randomize(solSize, min, max)
    population.population = evaluate(population, fitness)
        
    for (i <- 1 to maxIter) {
      println("Generation: " + i + " - " + population.getBest.fitness)
      var select = selection.select(population)
      var child = crossover.mate(select(0), select(1))
      println(child.sol)
      child = mutation.mutate(child)
      child.fitness = fitness.evaluate(child)
      println(child.sol)
      val (a, b) = population.population.splitAt(population.population.indexOf(select(2)))
      population.population = a ++ b.tail :+ child
      
    }
    
    
    population.getBest
    
  }
  
  private def evaluate(pop: Population, fitness: Fitness) = {
    pop.population map { x => x.setFitness(fitness.evaluate(x)) }
  }
}