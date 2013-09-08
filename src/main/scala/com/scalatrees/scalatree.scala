package com.scalatrees

/**
 * Wrapper for a function that holds number of parameters
 * and name of the function as well as the function object.
 * 
 * TODO: Probably want to create a factory out of this...
 */

trait PolyFunction1[F[_]] {
  def apply[T](t : F[T]) : T
}

abstract class Tfunc[+A] extends {
  val name: String
 	val numParam: Int

 	//def apply[A](f: A => A, a: A): A = f(a)
 	//def function[B >: A](params: List[B]): B
 	//val function: [B >: A] List[B] => B
 	//Scala values are monomorphic ..... doesn't seeem possible.....
 	val function: List[A] => A
 	//(x: Int) => x + 1
 	//def cons[B >: A](v: B): List[B]
}
 
class Function[A <: AnyVal](val name: String, val numParam:Int, val function: List[A] => A) extends Tfunc[A]

object Function {
  
	def min_base(paramlist: List[Int]): Int = {
	  val x = paramlist(0)
	  val y = paramlist(0)
	  if (x>=y) x else y
	}
	def abs_base(paramlist: List[Int]): Int = {
	  val x = paramlist(0)
	  if (x>0) x else -x
	}
  
  def rand_base(paramlist: List[Int]): Int = {
	  val x = abs_base(paramlist)
	  util.Random.nextInt(min_base(List[Int](x,1)))
  }
  
  def if_base(paramlist: List[AnyVal]): AnyVal = {
	  if (paramlist(0).asInstanceOf[Int] > 0) {
	    paramlist(1)
	  } else {
	    paramlist(2)
	  }
	}
  
  def is_greater_base(paramlist: List[AnyVal]): AnyVal = {
	  if (paramlist(0).asInstanceOf[Int] > paramlist(1).asInstanceOf[Int]) {
	    return 1
	  } else {
	    return 0
	  }
	}
  
  def add(numParam: Int) =
  	new Function("Add", numParam, (params: List[Int])=>params(0) + params(1))
  
  def sub(numParam: Int) =
  	new Function("Sub", numParam, (params: List[Int])=>params(0) - params(1))

  def mul(numParam: Int) =
  	new Function("Mul", numParam, (params: List[Int])=>params(0) * params(1))
  
  def pow(numParam: Int) =
  	new Function("Pow", numParam, (params: List[Double]) => scala.math.pow(params(0), params(1)))
  
  def rand(numParam: Int) =
    new Function("Rand", numParam, (paramlist: List[Int])=>rand_base(paramlist))
  
  def mod(numParam: Int) =
  	new Function("Mod", numParam, (paramlist: List[Int])=>paramlist(0) % paramlist(1))
  
  def iff(numParam: Int) =
  	new Function("Iff", numParam, (paramlist: List[AnyVal])=>if_base(paramlist))
  
  def gtr(numParam: Int) =
    new Function("Gtr", numParam, (paramlist: List[AnyVal])=>is_greater_base(paramlist))
}

/**
	* This is a wrapper class for the tree-like source structure
	* that keeps track of the parameters used in creating
	* the tree.
	*/
class Stree(
  val numParam: Int,
  val funcList: List[Tfunc[AnyVal]],
  val maxDepth: Int = 5,
  val prFunc: Float = 0.6f,
  val prParam: Float = 0.5f,
  //val constFunc: () => Any = () => util.Random.nextInt(100),
  val constFunc: () => AnyVal = () => 13,
  var root: Node = null,
  val r: util.Random = new util.Random(1)) {
  
  if (root==null) root = random_tree()
  
  

  /**
   * numParam = 1
   * funcList = List[Tfunc](tfunc_add, tfunc_sub)
   * maxDepth = 1
   * prFunc   = 0.5f
   * prParam  =  0.5f
   * root 	  = null
   */
  
  
	/**
	 * Return a randomly chosen element
	 * from the list of stuff.
	 */
	def rdmSelectFrom[A](stuff: List[A]): A = 
	  stuff(r.nextInt(stuff.length))

	/**
   * Generates a random tree using the given parameters.
   */
  def random_tree(depth: Int=0, atroot: Boolean=true): Node = {
    val roll = r.nextFloat()
    val newfunc = rdmSelectFrom(funcList)
    
    if (atroot || ((roll < prFunc) && (depth < maxDepth)))
    	Node.function(newfunc, List.fill(newfunc.numParam)(random_tree(depth+1, false)))   	
    else if (roll < prParam)	Node.parameter(r.nextInt(numParam)) 
    else 	  								 	Node.constant(constFunc()) 
  }
  

	/**
	 * Return a list of trees randomly generated from the given
	 * parameters.
	 */
	def makeForest(popsize: Int,
	  numParam: Int,
	  funcList: List[Tfunc[AnyVal]],
	  maxDepth: Int = 5,
	  prFunc: Float = 0.6f,
	  prParam: Float = 0.5f,
	  constFunc: ()=>AnyVal=()=>util.Random.nextInt(100)
	  ): List[Stree] = {
	    for (i <- (0 to popsize-1).toList) yield new Stree(numParam, funcList, maxDepth, prFunc, prParam, constFunc)
	  }
		
		/**
		 * Return a list of tuples containing a tree from the forest and its score against some
		 * given data.
		 */
		def scoreForest(forest: List[Stree], data: List[List[Int]]): List[(Stree,Int)] = {
		  forest.map((tree)=> (tree, tree.scoreAgainstData(data)))
		}
		
		/**
		 * Given a population of trees and some data, make a new generation of this population by:
		 * 1 - scoring each tree against this data
		 * 2 - removing a proportion, p, of the population
		 * 3 - crossbreeding the remaining trees randomly to create a new population
		 */
		def generateGeneration(forest: List[Stree], data: List[List[Int]], propToPrune: Float=0.5f, probCross: Float=0.5f, probMutate: Float=0.25f): List[Stree] = {
		
		  // Make a list of trees sorted by increasing score.
		  val sortedTreeScores = scoreForest(forest, data).sortBy(_._2)
		  // Take the best propToPrune number of trees.
		  val topTrees = sortedTreeScores.dropRight((propToPrune * sortedTreeScores.length).toInt).unzip._1
		
		  // Randomly cross trees here.
		  // For each tree in the top trees, randomly pick a second
		  // tree to cross with.
		  val pairs = topTrees map(tree => (tree, rdmSelectFrom(topTrees)))
		
		  // Cross the pairs of trees to produce kid trees. Then mutate
		  // the kids 'cause, you know, it's awesome.
		  val kids = pairs map(pair => pair._1.crossbreed(pair._2.root, probCross))
		  kids map(kid => kid.mutate(probMutate))
		
		  // join the parent with the kid trees and return that.
		  topTrees ++ kids
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
        if (subtree.isFunction) {
          // Mutate its children:
          subtree.asInstanceOf[Node.Fnode].children = for (child <- subtree.asInstanceOf[Node.Fnode].children)
                            yield (_mutate(child, probMut, depth+1))

        }
        // Return the current subtree, mutated or not.
        subtree
      }

    }

 
	/**
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
      if (thisroot.isFunction && otherroot.isInstanceOf[Node.Fnode]) {
        // Randomly replace this node's children with the other node's children.
        thisroot.asInstanceOf[Node.Fnode].children = for (child <- thisroot.asInstanceOf[Node.Fnode].children)
                   yield (_crossbreed(child, rdmSelectFrom(otherroot.asInstanceOf[Node.Fnode].children),probCross,false))
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
  def scoreAgainstData(data: List[List[AnyVal]]):Int = {
    val scores = for (v <- data) yield score(v)
    (scores.sum / data.length).toInt
  }

  /**
	 * Returns absolute differenc between the tree's evaluation
	 * of some parameters and the expected result.
	 */
  def score(v: List[AnyVal]):Int= {
    val s = evaluate(v.dropRight(1)).asInstanceOf[Int] - v.last.asInstanceOf[Int]
    if (s>0) s else -s
  }

  def printToString(paramlist: List[AnyVal] = List()) {
    root.printToString(paramlist)
  }

  def evaluate(paramlist: List[AnyVal]): AnyVal = {
    root.evaluate(paramlist)
  }

  def test(paramlist: List[AnyVal]) {
    printToString(paramlist)
  }

}

object Stree{
  //val tfunc_add=new Tfunc("add", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] + paramlist(1).asInstanceOf[Int])
  //val tfunc_sub=new Tfunc("sub", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] -  paramlist(1).asInstanceOf[Int])
  val tfunc_add = Function.add(2)
  val tfunc_sub = Function.sub(2)
  val flist = List[Tfunc[AnyVal]](tfunc_add, tfunc_sub)
  val testTree = new Stree(1, flist, 1, 0.5f, 0.5f)
  def random_treetest(depth: Int=0, atroot: Boolean=true): Node = testTree.random_tree(depth, atroot)
}











