package main.scala

import scala.util.control.Breaks._
import org.jgap._
import org.jgap.impl._
import main.java.GreedyMutation
import org.jgap.event.EventManager
import java.io.PrintWriter
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

object MainJGAP extends App {

  if (args.length != 7) {
    throw new IllegalArgumentException("Command line parameter expected: number of scrambles, population size, iteration number, crossover, greedy, mutation, repeats")
  }

  val file = "results/cubies_fitness/scrambles%d_pop%d_cross%.2f_greedy%d_mutation%d.txt".format(args(0).toInt, args(1).toInt, args(3).toDouble, args(4).toInt, args(5).toInt)

  //for (i <- 1 to args(6).toInt) {

    var rub = new RubiksCube

    val scramble = rub.scramble(args(0).toInt)
    println(rub)
    println(rub.isSolved)

    println(rub.copy)

    var conf: Configuration = new GAConfiguration(rub, args(3).toDouble, args(4).toInt, args(5).toInt)

    val sampleGenes: List[Gene] = (for (i <- 1 to 40) yield new IntegerGene(conf, 0, 17)).toList

    val sampleChromosome = new Chromosome(conf, sampleGenes.toArray)
    conf.setSampleChromosome(sampleChromosome)
    conf.setPopulationSize(args(1).toInt)

    var population: Genotype = Genotype.randomInitialGenotype(conf)

    val f = new File(file)
    f.getParentFile.mkdirs

    //val pw = new PrintWriter(new BufferedWriter(new FileWriter(f, true)))

    breakable(for (i <- 1 to args(2).toInt) {
      population.evolve();
      val fittest: IChromosome = population.getFittestChromosome

      println("Gen " + i + ": current best = " + fittest.getFitnessValue);

      //pw.write(i + "\t" + fittest.getFitnessValue + "\n")
      if (fittest.getFitnessValue >= 1.0) break
    })

    //pw.close

    println(population.getFittestChromosome)

    println(rub)
    val best = population.getFittestChromosome

    printCubeWithSolution(rub, best)


    def printCubeWithSolution(cube: RubiksCube, chromosome: IChromosome) = {
      val cubeCopy = cube.copy
      println(chromosome.getApplicationData)
      for (i <- 0 until chromosome.getApplicationData.asInstanceOf[Int]) {
        cubeCopy.rotateSide(chromosome.getGene(i).getAllele.asInstanceOf[Int])
      }
      println(cubeCopy)
    }

    println("THE SCRAMBLE SOLVED: ")
    println(RubiksCube.decode(scramble, scramble.length))

    println("STEPS TO REPRODUCE: ")
    println(RubiksCube.decode((best.getGenes map { x => x.getAllele.asInstanceOf[Int] }).toVector, best.getApplicationData.asInstanceOf[Int]))
  //}
}

class GAConfiguration(rubiksCube: RubiksCube, crossover: Double, greedy: Int, mut: Int) extends Configuration {
  Configuration.reset
  
  this.setBreeder(new GABreeder)
  this.setRandomGenerator(new StockRandomGenerator)
  this.setEventManager(new EventManager)
  this.setMinimumPopSizePercent(0)
  this.setSelectFromPrevGen(0.3)
  this.setKeepPopulationSizeConstant(true)
  this.setFitnessEvaluator(new DefaultFitnessEvaluator)
  this.setChromosomePool(new ChromosomePool)
  this.setPreservFittestIndividual(true)
  this.setFitnessFunction(new SideCompleteFitness(rubiksCube))
  this.addGeneticOperator(new CrossoverOperator(this, crossover))
  this.addGeneticOperator(new GreedyMutation(this, greedy))
  this.addGeneticOperator(new MutationOperator(this, mut))
  this.addNaturalSelector(new TournamentSelector(this, 3, 0.5), true)
}

class MaxCubiesFitness(val rubiksCube: RubiksCube) extends FitnessFunction {

  def evaluate(a_subject: IChromosome): Double = {

    var rubiks = rubiksCube.copy

    var fitness = 0.0;
    var index = 0;

    breakable {
      for (gene <- a_subject.getGenes) {
        val newFit = (evaluateCube(rubiks) + sumSides(rubiks)) / (3.0 * 48)

        if (newFit > fitness) {
          fitness = newFit
          index = a_subject.getGenes.indexOf(gene)
        }

        if (rubiks.isSolved) break
        rubiks.rotateSide(gene.getAllele.asInstanceOf[Int])

      }
    }

    a_subject.setApplicationData(index)
    fitness

  }

  def evaluateCube(cube: RubiksCube): Double = {
    var sum = 0.0

    var tmpSum = 0.0
    for (i <- 0 to 11) {
      if (cube.getEdgeCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum += 4 * tmpSum;

    tmpSum = 0.0;

    for (i <- 0 to 7) {
      if (cube.getCornerCubie(i).isCorrect(rubiksCube)) tmpSum += 1
    }

    sum + 6 * tmpSum
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

class SideCompleteFitness(val rubiksCube: RubiksCube) extends FitnessFunction {

  def evaluate(a_subject: IChromosome): Double = {

    var rubiks = rubiksCube.copy

    var fitness = 0.0;
    var index = 0;

    breakable {
      for (gene <- a_subject.getGenes) {
        val newFit = sumSides(rubiks) / 48

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
