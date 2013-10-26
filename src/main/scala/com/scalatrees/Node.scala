package com.scalatrees

/** Node Types
 * 
 * CNode: Constant node, holds a constant value, can return its value
 * FNode: Function node, holds a functions and nodes as parameters, can return its evaluated function
 */
sealed trait Node { def evaluate: Double }
case class CNode(value: Double) extends Node() { def evaluate: Double = value }
case class FNode(func: Operation, val children: List[Node]) extends Node {  def evaluate: Double = func.eval(children map {_.evaluate} ) }

/** Node Operations
 * 
 * Add: Add two nodes together
 * Mod: Take the modulo of two nodes
 * Subtract: Take the difference of two nodes
 * Multiple: Multiple two nodes together
 */
sealed trait Operation  { def eval(node: List[Double]): Double }
case object Add extends Operation { def eval(params: List[Double]) = params.reduce(_+_) }
case object Mod extends Operation { def eval(params: List[Double]) = params.reduce(_%_) }
case object Subtract extends Operation { def eval(params: List[Double]) = params.reduce(_-_) }
case object Multiply extends Operation { def eval(params: List[Double]) = params.reduce(_*_) }
//Power
//Min
//Max
//Absolute
//GreaterThan
//LessThen

