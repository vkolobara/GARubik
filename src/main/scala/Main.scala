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

object Main extends App {

  var rub = new RubiksCube

  rub.scramble(1)
  println(rub)
  println(rub.isSolved)

  var conf: Configuration = new DefaultConfiguration
  conf.setPreservFittestIndividual(true)
  conf.setKeepPopulationSizeConstant(true)
  conf.setFitnessFunction(new CubiesFitness(rub))

  val sampleGenes: List[Gene] = (for (i <- 1 to 40) yield new IntegerGene(conf, 0, 11)).toList

  val sampleChromosome = new Chromosome(conf, sampleGenes.toArray)
  conf.setSampleChromosome(sampleChromosome)

  conf.setPopulationSize(100)

  var population: Genotype = Genotype.randomInitialGenotype(conf)

  for (i <- 1 to 1000) {
    population.evolve();
    val fittest: IChromosome = population.getFittestChromosome

    println("Gen " + i + ": current best = " + fittest.getFitnessValue);
  }

  println(population.getFittestChromosome)

}

class CubiesFitness(val rubiksCube: RubiksCube) extends FitnessFunction {

  def evaluate(a_subject: IChromosome): Double = {
    
    var rubiks = rubiksCube.copy

    var fitness = 0.0;
    
    breakable {
      for (gene <- a_subject.getGenes) {
        rubiks.rotateSide(gene.getAllele.asInstanceOf[Int])
        fitness += evaluateCube(rubiks)
        if (rubiks.isSolved) break;
      }
    }
    println(rubiks)
    
    fitness/40.0;

  }

  def evaluateCube(cube: RubiksCube): Double = {
    var sum = 0.0

    var tmpSum = 0.0
    for (i <- 0 to 11) {
      if (rubiksCube.getEdgeCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum += 4 * tmpSum;

    tmpSum = 0.0;

    for (i <- 0 to 7) {
      if (rubiksCube.getCornerCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum + 6 * tmpSum
  }
}

class SideCompleteFitness(val rubiksCube: RubiksCube) extends FitnessFunction {

  private val maxFit = 40.0;

  def evaluate(a_subject: IChromosome): Double = {
    var fitness = maxFit;
    var rubiks = rubiksCube.copy

    breakable {
      for (gene <- a_subject.getGenes) {
        rubiks.rotateSide(gene.getAllele.asInstanceOf[Int])
        fitness = fitness - 1
        if (rubiks.isSolved) break;
      }
    }

    fitness + sumSides(rubiks);
  }

  def sumSides(cube: RubiksCube): Double = {
    var sum = 0.0;

    for (side <- cube.sides) {
      val center = side.elem(1)(1)
      for (row <- 0 to 2) {
        for (col <- 0 to 2)
          if (side.face(row)(col) == center) sum += 1
      }

    }

    sum
  }

}