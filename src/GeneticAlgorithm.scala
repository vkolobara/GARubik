

class GeneticAlgorithm(val maxIterC: Int, val popSizeC: Int, val selectionC: List[Selection], val mutationC: List[Mutation], val crossoverC: List[Crossover]){
  final val maxIter = maxIterC
  final val popSize = popSizeC
  final val selection = selectionC
  final val mutation = mutationC
  final val crossover = crossoverC
  
  
  def run(indSize: Int) = {
    
    
    
  }
  
}