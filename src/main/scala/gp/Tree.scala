package main.scala.gp
import main.scala.RubiksCube

abstract class Tree(value: Primitive) {
  
  def isEmpty: Boolean
  
  /*
   * Adds a new node to the tree
   */
  def add(value: Primitive)
  
  /*
   * Executes the tree and returns the output (max 40 moves)
   */
  def execute: Terminal = value.execute(this)
  
}

case class Leaf(value: Terminal) extends Tree(value) {
  def isEmpty = true
  
 }

case class Node(value: Function, children: Vector[Tree]) extends Tree(value) {
  def isEmpty = false
  
}

abstract class Primitive(val name: Int) {
  def execute(tree: Tree): Terminal
  def getName = name
  
}

abstract case class Function(val name: Int, val cube: RubiksCube) extends Primitive(name) {
  def execute(tree: Tree) = exec(tree)
  def exec(tree: Tree): Terminal
}

class EdgeFunction(val name: Int, val cube: RubiksCube, val index: Int) extends Function(name, cube) {
    def exec(tree: Node) =  if (cube.getEdgeCubie(index).isCorrect(cube)) tree.children(0).execute else tree.children(1).execute
}

class CubiesFunction(val name: Int, val cube: RubiksCube, val index: Int) extends Function(name, cube) {
    def exec(tree: Node) =  if (cube.getCornerCubie(index).isCorrect(cube)) tree.children(0).execute else tree.children(1).execute
}

case class Terminal(val name: Int) extends Primitive(name) {
  def execute(tree: Tree) = this
}

