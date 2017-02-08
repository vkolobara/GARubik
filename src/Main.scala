

object Main extends App {
  var pop = new Population(100);
  pop.initPopulation(10);
  println(pop.population.length);
}