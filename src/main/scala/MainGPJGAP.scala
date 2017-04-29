package main.scala

import org.jgap.gp.impl.GPConfiguration
import org.jgap.gp.CommandGene
import main.java.IfEdgeCubieCorrectElse
import main.java.IfCornerCubieCorrectElse
import org.jgap.gp.terminal.Terminal
import org.jgap.gp.impl.GPGenotype
import org.jgap.gp.GPFitnessFunction
import org.jgap.gp.IGPProgram
import scala.util.control.Breaks._
import org.jgap.gp.impl.DeltaGPFitnessEvaluator


object MainGPJGAP extends App {

  var rub = new RubiksCube
  rub.scramble(50)
  
  val conf = GPRubik.createGPConfiguration(rub)
  var gp = GPRubik.createGenotype(conf)
  
  gp.evolve(1000)
  
  println(gp.getAllTimeBest.getFitnessValue)
  
  
}

object GPRubik {
  def createGenotype(conf: GPConfiguration): GPGenotype = {
    val types: Array[Class[_]] = Array(CommandGene.IntegerClass)
    val argTypes: Array[Array[Class[_]]] = Array(Array())
    
    val ifEdgeCubie = new IfEdgeCubieCorrectElse(conf, 0)
    val ifCornerCubie = new IfCornerCubieCorrectElse(conf, 0)
    val term = new Terminal(conf, CommandGene.IntegerClass, 0, 17, false)
    
    val nodes = (ifEdgeCubie :: ifCornerCubie :: term :: Nil).toArray[CommandGene]
    
    val allNodes = new Array[Array[CommandGene]](1)
    allNodes(0) = nodes
    
    GPGenotype.randomInitialGenotype(conf, types, argTypes, allNodes, 100, true)
    
  }
  
  def createGPConfiguration(rub: RubiksCube): GPConfiguration = {
    var conf = new GPConfiguration
    conf.setGPFitnessEvaluator(new DeltaGPFitnessEvaluator)
    conf.setMaxInitDepth(7)
    conf.setPopulationSize(100)
    conf.setFitnessFunction(new GPRubikFitnessFunction(rub))
    conf
  }
}

class GPRubikFitnessFunction(val rub: RubiksCube) extends GPFitnessFunction { 
  
  def evaluate(program: IGPProgram): Double = {
    var maxFitness = 0.0
    val cube = rub.copy
    breakable (
    for (i <- 0 to 40) {
    	val fitness = sumSides(cube)
    	if (fitness > maxFitness) maxFitness = fitness
    	if (cube.isSolved) break
      program.setApplicationData(cube)
      cube.rotateSide(program.execute_int(0, new Array[Object](0)))
    })
    
    maxFitness 
    
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