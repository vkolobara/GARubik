package main.scala.ga.operators

import scala.util.Random
import java.util.Arrays

trait Mutation {
  def mutate(sol: Solution): Solution
}

trait Crossover {
  def mate(parent1: Solution, parent2: Solution): Solution
}

trait Selection {
  def select(population: Population): Vector[Solution]
}

class OnePointCrossover extends Crossover {
  private val rand: Random = new Random

  def mate(parent1: Solution, parent2: Solution): Solution = {
    var child = parent1.duplicate

    val index = rand.nextInt(parent1.size - 2) + 1

    child.sol = (parent1.sol take index) ++ (parent2.sol takeRight parent2.sol.size - index)

    child

  }
}

class TwoPointCrossover extends Crossover {
  private val rand: Random = new Random

  def mate(parent1: Solution, parent2: Solution): Solution = {
    var child = parent1.duplicate

    val index = rand.nextInt(parent1.size / 2)
    val index2 = rand.nextInt(parent1.size / 2) + index

    child.sol = (parent1.sol take index) ++ (parent2.sol drop index take index2 - index) ++ (parent1.sol takeRight parent1.sol.size - index2)

    child

  }
}

class SimpleMutation(mutRate: Double) extends Mutation {
  private val rand: Random = new Random

  def mutate(sol: Solution): Solution = {
    var mutation = sol.duplicate

    mutation.sol = (for {
      i <- 0 until mutation.size
    } yield mut(sol.min, sol.max, mutation.sol(i))).toVector

    mutation

  }
  private def randBit(min: Int, max: Int) =
    rand.nextInt(max - min) + min

  private def mut(min: Int, max: Int, bit: Int): Int = {
    if (rand.nextDouble() <= mutRate) randBit(min, max) else bit
  }
}

class GreedyMutation extends Mutation {
  private val rand: Random = new Random

  def mutate(sol: Solution): Solution = {
    var mutation = sol.duplicate

    mutation.sol = (mutation.sol take sol.bestIndex) ++ (for {
      i <- sol.bestIndex until mutation.size
    } yield randBit(sol.min, sol.max)).toVector

    mutation

  }
  private def randBit(min: Int, max: Int) =
    rand.nextInt(max - min) + min

}

class TournamentSelection(k: Int) extends Selection {
  private val rand: Random = new Random

  def select(population: Population): Vector[Solution] = {
    var tournament = new Population(k)
    tournament.population =
      (for {
        i <- 1 to k
      } yield population.population(rand.nextInt(population.size))).toVector
    tournament.population.sortBy(_.fitness).reverse
  }
}

class RouletteWheelSelection(k: Int) extends Selection {
  private val rand: Random = new Random

  def select(population: Population): Vector[Solution] = {
    var tournament = new Population(k)

    var cumulativeFitness: Array[Double] = new Array(population.size)

    cumulativeFitness(0) = population.population(0).fitness

    for (i <- 1 until population.size) {
      cumulativeFitness(i) = cumulativeFitness(i - 1) + population.population(i).fitness
    }

      for (i <- 1 to k) {
        val randFit = rand.nextDouble * cumulativeFitness(cumulativeFitness.length - 1)
        var index = Arrays.binarySearch(cumulativeFitness, randFit)
        if (index < 0) index = Math.abs(index+1) 
        
        tournament.population = tournament.population :+ population.population(index)

      } 

    tournament.population.sortBy(_.fitness).reverse
  }
}



