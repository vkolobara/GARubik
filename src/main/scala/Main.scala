package main.scala

import main.scala.ga.operators.GreedyMutation
import main.scala.ga.operators.MaxCubiesFitness
import main.scala.ga.operators.SteadyStateAlgorithm
import main.scala.ga.operators.TournamentSelection
import main.scala.ga.operators.TwoPointCrossover
import main.scala.ga.operators.SideCompleteFitness
import main.scala.ga.operators.RouletteWheelSelection
import main.scala.ga.operators.OnePointCrossover

object Main extends App {

  var rub = new RubiksCube

  val scramble = rub.scramble(5)
  println(rub)
  println(rub.isSolved)
 
  val crossover = new TwoPointCrossover
  val mutation = new GreedyMutation
  val selection = new TournamentSelection(3)
  
  val steadyState = new SteadyStateAlgorithm(crossover, mutation, selection, 100000);
  
  val fitness = new SideCompleteFitness(rub)
  println(fitness.sumSides(rub))
  val popSize = 50
  val solSize = 24
  val min = 0 
  val max = 18
    
  val best = steadyState.run(fitness, popSize, solSize, min, max)

  println(best.fitness)
  println(best.sol)
  
  println("RUBIKS CUBE SOLUTION: ")
  rub.printCubeWithSolution(best)
  
  println("THE SCRAMBLE SOLVED: ")
  println(rub.decode(scramble, scramble.length))
  
  println("STEPS TO REPRODUCE: ")
  println(rub.decode(best.sol, best.bestIndex))
  
}


