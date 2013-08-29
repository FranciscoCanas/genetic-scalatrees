package com.scalatrees

package object stree {

/**
 * This is a wrapper class for the tree-like source structure
 * that keeps track of the parameters used in creating
 * the tree.
 */
class Stree(
  val numParam: Int,
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
    def random_tree(depth: Int=0,atroot: Boolean=true): Node = {
      val roll = util.Random.nextFloat()

      if (atroot || ((roll < prFunc) && (depth < maxDepth))) {
          // make a function node here, and recurse.
          val newfunc = choice(funcList)

          // Recusively create children subtrees.
          var children = for (i <- 1 to newfunc.numParam)
                            yield random_tree(depth+1,false)

          // Wrap it up in an fnode and return.
          new Fnode(newfunc, children.toList.asInstanceOf[List[Node]])

        } else if (roll < prParam) {
          // Make a parameter node.
          new Pnode(util.Random.nextInt(numParam))

        } else {
          // Make a constant node.
          new Cnode(constFunc())
        }
    }

    /**
     * Recurses through the tree and randomly mutates 
     * its subtrees.
     */
    def mutate(probMut: Float=0.15f){
      root = _mutate(root, probMut)
    }

    def _mutate(subtree: Node, probMut: Float=0.15f, depth: Int=0): Node = {
      if (util.Random.nextFloat() < probMut) {
        // Return a brand new subtree.
        random_tree(depth)
      } else {
        // If this is a function node:
        if (subtree.isInstanceOf[Fnode]) {
          // Mutate its children:
          subtree.asInstanceOf[Fnode].children = for (child <- subtree.asInstanceOf[Fnode].children)
                            yield (_mutate(child, probMut, depth+1))

        }
        // Return the current subtree, mutated or not.
        subtree
      }

    }

    /**
     * Recurses through this and an 'other' tree, randomly replaces 
     * subtrees on this tree with subtrees from the other.
     */
    def crossbreed(otherroot: Node, probCross: Float=0.15f): Stree = {
      var newroot = _crossbreed(root, otherroot, probCross)
      new Stree(numParam, funcList, maxDepth, prFunc, prParam, constFunc, newroot)
    }

    def _crossbreed(thisroot: Node, otherroot: Node, probCross: Float=0.15f, atroot: Boolean=true): Node = {
      if ((!atroot)&& (util.Random.nextFloat() < probCross)) {
        // Cross these trees
        otherroot
      } else {
        // See about crossing the childrens, if any:
        if (thisroot.isInstanceOf[Fnode] && otherroot.isInstanceOf[Fnode]) {
          // Randomly replace this node's children with the other node's children.
          thisroot.asInstanceOf[Fnode].children = for (child <- thisroot.asInstanceOf[Fnode].children)
                     yield (_crossbreed(child, choice(otherroot.asInstanceOf[Fnode].children),probCross,false))
        }
      // Return the current root, whether crossed or not.
      thisroot
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
class Tfunc(val name: String, val numParam:Int, val function: List[Any]=>Any){

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
