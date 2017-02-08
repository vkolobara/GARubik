import Action._

class Individual(val sizeC:Int) {
  
  val rand: util.Random = new util.Random;
  final val size: Int = sizeC;
  var fitness: Double = 0;
  var solution: List[Action] = Nil; 
  
  def randomize(): Individual = {
    solution = Stream.continually(Action(rand.nextInt((Action.maxId)))).take(size).toList
    return this
  }
  
}