package main.scala

import scala.util.control.Breaks._
import org.jgap._
import org.jgap.impl._
import main.java.GreedyMutation
import main.scala.MainJGAP.{conf, rub}
import org.jgap.event.EventManager

object MainJGAP extends App {

  if (args.length != 1) {
    throw new IllegalArgumentException("Command line parameter expected: number of scrambles")
  }

  var rub = new RubiksCube

  val scramble = rub.scramble(args(0).toInt)
  println(rub)
  println(rub.isSolved)

  println(rub.copy)

  var conf: Configuration = new GAConfiguration(rub)

  val sampleGenes: List[Gene] = (for (i <- 1 to 40) yield new IntegerGene(conf, 0, 17)).toList

  val sampleChromosome = new Chromosome(conf, sampleGenes.toArray)
  conf.setSampleChromosome(sampleChromosome)
  conf.setPopulationSize(50)

  var population: Genotype = Genotype.randomInitialGenotype(conf)

breakable(for (i <- 1 to 1000) {
    population.evolve();
    val fittest: IChromosome = population.getFittestChromosome

    println("Gen " + i + ": current best = " + fittest.getFitnessValue);
    if (fittest.getFitnessValue >= 48.0) break
  }
)

  println(population.getFittestChromosome)

  println(rub)

  printCubeWithSolution(rub, population.getFittestChromosome)

  val best = population.getFittestChromosome
  
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

}


class GAConfiguration(rubiksCube: RubiksCube) extends Configuration {

  this.setBreeder(new GABreeder)
  this.setRandomGenerator(new StockRandomGenerator)
  this.setEventManager(new EventManager)
  this.setMinimumPopSizePercent(0)
  this.setSelectFromPrevGen(0.6)
  this.setKeepPopulationSizeConstant(true)
  this.setFitnessEvaluator(new DefaultFitnessEvaluator)
  this.setChromosomePool(new ChromosomePool)
  this.setPreservFittestIndividual(true)
  this.setKeepPopulationSizeConstant(true)
  this.setFitnessFunction(new SideCompleteFitness(rubiksCube))
  this.addGeneticOperator(new CrossoverOperator(this, 0.5))
  this.addGeneticOperator(new GreedyMutation(this, 1))
  this.addGeneticOperator(new MutationOperator(this, 10))
  this.addNaturalSelector(new TournamentSelector(this, 5, 0.7), true)

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
