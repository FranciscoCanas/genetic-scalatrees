package com.scalatrees

/** THINGS MARKED WITH FAIL ARE FAILED GENERALIZATION ATTEMPTS
 * Possible node representations:
 * Fnode: Function node
 * 
 * Pnode: Parameter node
 * 				A tree node that holds a parameter
 *     		The index of the parameter, not its literal value
 *       
 * Cnode: Constant node
 * 			  A tree node that holds a constant value.
 */
abstract class Node {
  val spacer = " "
  val noder =   "\\"
  val stemmer = " |"
  
  /**
   * Evaluated your node.
   * Functions nodes will generate the result to your function
   * Parameter nodes will return the parameter it holds
   * Constant nodes will return its constant value
   */
  def evaluate(paramlist: List[AnyVal]): AnyVal
  
  def isFunction: Boolean 	= false
  def isConstant: Boolean		= false
  def isParameter: Boolean	= false
  def printToString(paramlist: List[AnyVal], indent: String=" ") { print(indent) }
}

object Node {

	//FAIL: class Fnode(func: Tfunc[AnyVal], var children: List[Node]) extends Node {
  class Fnode(func: Tfunc, var children: List[Node]) extends Node {
	  override def isFunction: Boolean 	= true
	  val name 		 = func.name

	  def evaluate(paramlist: List[AnyVal]): AnyVal =
	   func.function(
	     for (child <- children)
	       yield child.asInstanceOf[Node].evaluate(paramlist))
	        

	  override def printToString(paramlist: List[AnyVal], indent: String=" ") {
	    super.printToString(paramlist, indent)
	    println(noder + name + "=" + evaluate(paramlist))
	    for (child <- children.dropRight(1)) child.printToString(paramlist, indent + spacer * 2 + stemmer)
	    children.last.printToString(paramlist, indent + spacer * 4)
	  }
	}
	
	private class Pnode(paramId: Int) extends Node() { 
	  override def isParameter: Boolean = true
	  
	  def evaluate(paramlist: List[AnyVal]): AnyVal =
	    paramlist(paramId).asInstanceOf[AnyVal]

	  def paramToString(id: Int): String = "p[" + id + "]"
	  
	  override def printToString(paramlist: List[AnyVal], indent: String=" ") {
	    super.printToString(paramlist, indent)
	    println(noder + paramToString(paramId) + "=" + evaluate(paramlist))
	  }
	}

	private class Cnode(value: AnyVal) extends Node() {
	  override def isConstant: Boolean = true
	  
	  def evaluate(paramlist: List[AnyVal]): AnyVal = value
	  
	  override def printToString(paramlist: List[AnyVal], indent: String=" ") {
	    super.printToString(paramlist, indent)
	    println(noder + value)
	  }
	}
	
  //FAIL: def function(func: Tfunc[AnyVal], children: List[Node]): Node =
	def function(func: Tfunc, children: List[Node]): Node =
    new Fnode(func, children)
	  
  def parameter(paramId: Int): Node =
    new Pnode(paramId)
  
  def constant(value: AnyVal): Node =
    new Cnode(value)
}