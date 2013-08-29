package com.scalatrees

package object stree {

/**
 * This is a wrapper class for the tree-like source structure
 * that keeps track of the parameters used in creating
 * the tree.
 */
class Stree(
  val numparam: Int,
  val funcList: List[Tfunc],
  val maxDepth: Int = 5,
  val prFunc: Float = 0.6f,
  val prParam: Float = 0.5f,
  val constFunc: ()=>Any=()=>util.Random.nextInt(100),
  var root: Node = null ) {

    if (root==null) {
      root = random_tree()
    }

    /**
     * Generates a random tree using the given parameters.
     */
    def random_tree(depth: Int=0): Node = {
      val roll = util.Random.nextFloat()

      if ((roll < prFunc) && (depth < maxDepth)) {
          // make a function node here, and recurse.
          val newfunc = choice(funcList)

          // Recusively create children subtrees.
          var children = for (i <- 1 to newfunc.numparam)
                            yield random_tree(depth+1)

          // Wrap it up in an fnode and return.
          new Fnode(newfunc, children.toList.asInstanceOf[List[Node]])

        } else if (roll < prParam) {
          // Make a parameter node.
          new Pnode(util.Random.nextInt(numparam))

        } else {
          // Make a constant node.
          new Cnode(constFunc())
        }
    }

    def printToString(paramlist: List[Any]) {
      root.printToString(paramlist)
    }

    def evaluate(paramlist: List[Any]) {
      root.evaluate(paramlist)
    }

    def test(paramlist: List[Any]) {
      printToString(paramlist)
    }
  }

/**
 * Represents a particular object in the source tree: could be
 * a function, a parameter, or a constant.
 */
abstract class Node() {
   def printToString(paramlist: List[Any], indent: Int=0)
   def evaluate(paramlist: List[Any]): Any
  }

/**
 * Wrapper for a function that holds number of parameters
 * and name of the function as well as the function object.
 */
class Tfunc(val name: String, val numparam:Int, val function: List[Any]=>Any){

}

/**
 * A tree node that contains a function.
 */
class Fnode(val func: Tfunc, var children: List[Node]) extends Node() {
  val name = func.name
  val function = func.function

  /**
   * Recursively evaluate the children of this function,
   * and evaluate this function to get the result.
   */
   def evaluate(paramlist: List[Any]): Any =
    function(
      for (child <- children)
        yield child.asInstanceOf[Node].evaluate(paramlist)
           )
  /**
   * Prints the name of this function and recusively
   * prints its children.
   */
  def printToString(paramlist: List[Any], indent: Int=0) {
    //(0 to indent).foreach(print(' '))
    for(i <- 0 to indent) print("-")
    println(name+"="+evaluate(paramlist))
    for (child <- children) yield child.printToString(paramlist, indent+4)
  }

}

/**
 * A tree node that holds a parameter: ie, the
 * index of the parameter, not its literal value.
 */
class Pnode(paramid: Int) extends Node() {

  /**
   * Return the value of this parameter.
   */
  def evaluate(paramlist: List[Any]): Any =
    paramlist(paramid).asInstanceOf[Any]

  /**
   * Prints the parameter index and its value.
   */
  def printToString(paramlist: List[Any], indent: Int = 0) {
    for (i <- 0 to indent) print("-")
    println(paramid + "=" + evaluate(paramlist))
  }
}

/**
 *A tree node that holds a constant value.
 */
class Cnode(value: Any) extends Node() {

  /**
   * Return the value of this constant.
   */
  def evaluate(paramlist: List[Any]): Any =
    value

  /**
   * Prints the constant.
   */
  def printToString(paramlist: List[Any], indent: Int = 0) {
    for(i <- 0 to indent) print("-")
    println(value)
  }
}

/**
 * Useful Utility Functions
 */

/**
 * Return a randomly chosen element
 * from the list of stuff.
 */
def choice[A](stuff: List[A]): A = {
  stuff(util.Random.nextInt(stuff.length))

}
}
