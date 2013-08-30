package com.scalatrees

/**
* Wrapper for a function that holds number of parameters
* and name of the function as well as the function object.
*/
class Tfunc(val name: String, val numParam:Int, val function: List[Any]=>Any)

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
  val constFunc: () => Any = () => util.Random.nextInt(100),
  var root: Node = null ) {
  
  if (root==null) root = random_tree()
  
	/**
	 * Return a randomly chosen element
	 * from the list of stuff.
	 */
	def choice[A](stuff: List[A]): A = 
	  stuff(util.Random.nextInt(stuff.length))

  
  

  /**
   * Generates a random tree using the given parameters.
   */
  def random_tree(depth: Int=0, atroot: Boolean=true): Node = {
    val roll = util.Random.nextFloat()

    if (atroot || ((roll < prFunc) && (depth < maxDepth))) {
      val newfunc = choice(funcList) // make a function node here, and recurse.
	
	    var children = // Recusively create children subtrees.
	      for (i <- 1 to newfunc.numParam)
	      	yield random_tree(depth+1, false)
	
	    // Wrap it up in an fnode and return.
	    new Fnode(newfunc, children.toList.asInstanceOf[List[Node]])

    } 
    else if (roll < prParam) {
	    new Pnode(util.Random.nextInt(numParam)) // Make a parameter node.
	  } 
    else {
	    new Cnode(constFunc()) // Make a constant node.
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

    /**
* Evaluates this source tree against a list of list containing parameters and their
* expected output. Returns a score based on the tree's performance.
*
* data should be of the form:
* ((x11,x12,x13...y1),
* (x21,x22... y2),
* ...
* (xn1, xn2... yn))
*/
    def scoreAgainstData(data: List[List[Any]]):Int = {
      val scores = for (v <- data) yield score(v)
      scores.sum
    }

    /**
* Returns absolute differenc between the tree's evaluation
* of some parameters and the expected result.
*/
    def score(v: List[Any]):Int= {
      val s = evaluate(v.dropRight(1)).asInstanceOf[Int]- v.last.asInstanceOf[Int]
      if (s>0) s else -s
    }


    def printToString(paramlist: List[Any]) {
      root.printToString(paramlist)
    }

    def evaluate(paramlist: List[Any]): Any = {
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
  def printToString(paramlist: List[Any], indent: Int=0) {
    for(i <- 0 to indent-1) print(" ")
    print("|")
    for(i <- 0 to 3) print("-")
  }
   def evaluate(paramlist: List[Any]): Any
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
  override def printToString(paramlist: List[Any], indent: Int=0) {
    super.printToString(paramlist, indent)
    println(name + "=" + evaluate(paramlist))
    for (child <- children) yield child.printToString(paramlist, indent+6)
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
  override def printToString(paramlist: List[Any], indent: Int = 0) {
    super.printToString(paramlist, indent)
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
  override def printToString(paramlist: List[Any], indent: Int = 0) {
    super.printToString(paramlist, indent)
    println(value)
  }
}

/**
* Useful Utility Functions
*/

