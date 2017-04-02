package main.scala.ga.operators

class SteadyStateAlgorithm(crossover: Crossover, mutation: Mutation, selection: Selection, maxIter: Int) {

  def run(fitness: Fitness, popSize: Int, solSize: Int, min: Int, max: Int): Solution = {
    var population = new Population(popSize)
    population.population = evaluate(population.randomize(solSize, min, max), fitness)

    var iterWithoutProgress = 0
    var currBest = population.getBest.fitness

    for (i <- 0 to maxIter) {
      if (i % 1000 == 0)
        println("Generation: " + i/popSize + " - " + population.getBest.fitness)

      var select = selection.select(population)
      var child = mutation.mutate(crossover.mate(select(0), select(1)))
      child.fitness = fitness.evaluate(child)

      val (a, b) = population.population.splitAt(population.population.indexOf(select(2)))
      population.population = a ++ b.tail :+ child

      if (currBest < population.getBest.fitness) {
        iterWithoutProgress = -1
        currBest = population.getBest.fitness
      }

//      if (iterWithoutProgress == popSize * 1000) {
//        population.population = evaluate(population.randomize(solSize, min, max), fitness)
//        iterWithoutProgress = -1
//      }

      iterWithoutProgress += 1

    }

    population.getBest

  }

  private def evaluate(pop: Vector[Solution], fitness: Fitness) = {
    pop map { x => x.setFitness(fitness.evaluate(x)) }
  }
}