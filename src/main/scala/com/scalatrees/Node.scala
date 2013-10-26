package com.scalatrees

/** Node Types
 * 
 * CNode: Constant node, holds a constant value, can return its value
 * FNode: Function node, holds a functions and nodes as parameters, can return its evaluated function
 */
sealed trait Node { def evaluate: Int }
case class CNode(value: Int) extends Node() { def evaluate: Int = value }
case class FNode(func: Operation, val children: List[Node]) extends Node {  def evaluate: Int = func.eval(children map {_.evaluate} ) }

/** Node Operations
 * 
 * Add: Add two nodes together
 * Mod: Take the modulo of two nodes
 * Subtract: Take the difference of two nodes
 * Multiple: Multiple two nodes together
 */
sealed trait Operation  { def eval(node: List[Int]): Int }
case object Add extends Operation { def eval(params: List[Int]) = params.reduce(_+_) }
case object Mod extends Operation { def eval(params: List[Int]) = params.reduce(_%_) }
case object Subtract extends Operation { def eval(params: List[Int]) = params.reduce(_-_) }
case object Multiply extends Operation { def eval(params: List[Int]) = params.reduce(_*_) }
//Power
//Min
//Max
//Absolute
//GreaterThan
//LessThen

