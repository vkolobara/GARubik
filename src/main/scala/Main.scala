
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

  var x = new Face(0)
  
  x.face(0) = (1 to 3).toArray
  x.face(1) = (4 to 6).toArray
  x.face(2) = (7 to 9).toArray
  
  println(x)
  println
  x.rotate(true)
  println(x)
  x.rotate(false)
  println
  println(x)
 
  var rub = new RubiksCube
  println(rub.isSolved)


  var conf: Configuration = new DefaultConfiguration
  conf.setPreservFittestIndividual(true)
  conf.setKeepPopulationSizeConstant(true)
  conf.setFitnessFunction(new TestFitness)

  val sampleGenes: List[Gene] = (for (i <- 0 to 40) yield new IntegerGene(conf, 0, 12)).toList
  
  val sampleChromosome = new Chromosome(conf, sampleGenes.toArray)
  conf.setSampleChromosome(sampleChromosome)

  conf.setPopulationSize(100)

  var population: Genotype = Genotype.randomInitialGenotype(conf)

}

class TestFitness extends FitnessFunction {

  private val maxFit = 40;

  def evaluate(a_subject: IChromosome): Double = {

    return 0;
  }

}