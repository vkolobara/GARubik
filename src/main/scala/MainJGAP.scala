package main.scala

import scala.util.control.Breaks._;

import org.jgap.Configuration
import org.jgap.impl.DefaultConfiguration
import org.jgap.impl.IntegerGene
import org.jgap.Gene
import org.jgap.Chromosome
import org.jgap.Genotype
import org.jgap.IChromosome
import org.jgap.FitnessFunction
import org.jgap.impl.MutationOperator
import org.jgap.Population
import org.jgap.RandomGenerator
import main.java.GreedyMutation

object MainJGAP extends App {
  var rub = new RubiksCube

  val scramble = rub.scramble(10)
  println(rub)
  println(rub.isSolved)

  println(rub.copy)

  var conf: Configuration = new DefaultConfiguration
  conf.setPreservFittestIndividual(true)
  conf.setKeepPopulationSizeConstant(true)
  conf.setFitnessFunction(new SideCompleteFitness(rub))
  conf.addGeneticOperator(new GreedyMutation(conf, 30))

  val sampleGenes: List[Gene] = (for (i <- 1 to 40) yield new IntegerGene(conf, 0, 17)).toList

  val sampleChromosome = new Chromosome(conf, sampleGenes.toArray)
  conf.setSampleChromosome(sampleChromosome)

  conf.setPopulationSize(50)

  var population: Genotype = Genotype.randomInitialGenotype(conf)

  for (i <- 1 to 10000) {
    population.evolve();
    val fittest: IChromosome = population.getFittestChromosome

    println("Gen " + i + ": current best = " + fittest.getFitnessValue);
    if (fittest.getFitnessValue >= 48.0) break
  }

  println(population.getFittestChromosome)

  println(rub)

  printCubeWithSolution(rub, population.getFittestChromosome)

  val best = population.getFittestChromosome
  
  def printCubeWithSolution(cube: RubiksCube, chromosome: IChromosome) = {
    var cubeCopy = cube.copy
    println(chromosome.getApplicationData)
    for (i <- 0 until chromosome.getApplicationData.asInstanceOf[Int]) {
      cubeCopy.rotateSide(chromosome.getGene(i).getAllele.asInstanceOf[Int])
    }
    println(cubeCopy)
  }
  
    
  println("THE SCRAMBLE SOLVED: ")
  println(rub.decode(scramble, scramble.length))
  
  println("STEPS TO REPRODUCE: ")
  println(rub.decode((best.getGenes map { x => x.getAllele.asInstanceOf[Int] }).toVector, best.getApplicationData.asInstanceOf[Int]))
  
}



class SideCompleteFitness(val rubiksCube: RubiksCube) extends FitnessFunction {

  def evaluate(a_subject: IChromosome): Double = {

    var rubiks = rubiksCube.copy
    
    var fitness = 0.0;
    var index = 0;

    breakable {
      for (gene <- a_subject.getGenes) {
         val newFit = sumSides(rubiks)

        if (newFit > fitness) {
          fitness = newFit
          index = a_subject.getGenes.indexOf(gene)
        }
         if (rubiks.isSolved) break;
        rubiks.rotateSide(gene.getAllele.asInstanceOf[Int])
      }
    }

    a_subject.setApplicationData(index)
    fitness
  }

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
