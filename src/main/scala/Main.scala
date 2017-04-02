package main.scala

import scala.util.control.Breaks._;
import org.jgap._;
import org.jgap.impl.IntegerGene
import org.jgap.impl.DefaultConfiguration
import org.jgap.impl.IntegerGene
import org.jgap.Genotype
import org.jgap.Population
import org.jgap.FitnessFunction
import org.jgap.IChromosome

import Array._
import main.scala.ga.operators.SteadyStateAlgorithm
import main.scala.ga.operators.OnePointCrossover
import main.scala.ga.operators.SimpleMutation
import main.scala.ga.operators.TournamentSelection
import main.scala.ga.operators.SideCompleteFitness
import main.scala.ga.operators.CubiesFitness
import main.scala.ga.operators.MaxCubiesFitness
import main.scala.ga.operators.SideCompleteFitness
import main.scala.ga.operators.MaxCubiesFitness
import main.scala.ga.operators.GreedyMutation

object Main extends App {

  var rub = new RubiksCube

  rub.scramble(1)
  println(rub)
  println(rub.isSolved)

  val crossover = new OnePointCrossover
  val mutation = new GreedyMutation
  val selection = new TournamentSelection(3)
  
  val steadyState = new SteadyStateAlgorithm(crossover, mutation, selection, 100);
  
  val fitness = new MaxCubiesFitness(rub)
  val popSize = 50
  val solSize = 40
  val min = 0 
  val max = 12
    
  val best = steadyState.run(fitness, popSize, solSize, min, max)

  println(best.fitness)
  
}


