

class Population(val sizeC: Int) {
  final val size: Int = sizeC
  var population: List[Individual] = Nil

  def addIndividual(ind: Individual) = ind :: population
  def removeIndividual(ind: Individual) = { population = population.filter(_ != ind) }
  def getBest(): Individual = population.reduceLeft((i1, i2) => if (i1.fitness > i2.fitness) i1 else i2)
  def initPopulation(indSize: Int) = population = Stream.continually(new Individual(indSize).randomize()).take(size).toList
}