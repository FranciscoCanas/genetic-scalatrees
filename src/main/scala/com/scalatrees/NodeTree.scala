package com.scalatrees

class NodeTree(
  val funcList: List[Operation],
  val numParam: Int = 2,
  val maxDepth: Int = 5,
  val prFunc: Float = 0.5f,                        
  val randomGenerator: util.Random = new util.Random()) {      
  
  val root = randomTree()
  
	/** Return a randomly chosen element from the list of stuff. */
	def rdmSelectFrom[A](stuff: List[A]): A = 
	  stuff(randomGenerator.nextInt(stuff.length))

	/** Generates a random tree using the given parameters. */
  def randomTree(currentDepth: Int = 0): Node = {
    lazy val newFunc = rdmSelectFrom(funcList)
    if (currentDepth == 0 || 
    ((randomGenerator.nextFloat() < prFunc) && (currentDepth < maxDepth)))                  
    	FNode(newFunc, List.fill(2)(randomTree(currentDepth+1)))        
    else
      CNode(randomGenerator.nextInt(10))
  }
  
  def score: Double = root.evaluate
  override def toString = { root.toString() }
}

class NodeForest( 
  popSize: Int,
  funcList: List[Operation],
  numParam: Int =2,
  maxDepth: Int = 5,
  prFunc: Float = 0.5f,
  randomGenerator: util.Random = new util.Random()){
 
  val forest = List.fill(popSize)(new NodeTree(funcList, numParam, maxDepth, prFunc, randomGenerator))
  
  def rdmSelectFrom[A](stuff: List[A]): A = 
    stuff(randomGenerator.nextInt(stuff.length))
    
  /** Return a list of tuples containing a tree from the forest and its score against some given data. */
  def scoreForest: List[(NodeTree, Double)] = forest
    .map (tree => (tree, tree.score))     
    .sortBy(- _._2)                       //Sort in descending order
  
  def generateGeneration(deathRate: Float = 0.5f): NodeForest = {
    val mates = forest map { tree => (tree, rdmSelectFrom(forest)) }
    val forestStats = this.scoreForest
    this
  }
  override def toString = forest map ("\n" + _.toString) toString
    
}

	  


		/**
		 * Given a population of trees and some data, make a new generation of this population by:
		 * 1 - scoring each tree against this data
		 * 2 - removing a proportion, p, of the population
		 * 3 - crossbreeding the remaining trees randomly to create a new population
		 
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
  */



   /**
		* Recurses through the tree and randomly mutates
		* its subtrees.
		
    def mutate(probMut: Float=0.15f){
      root = _mutate(root, probMut)
    }

    def _mutate(subtree: Node, probMut: Float=0.15f, depth: Int=0): Node = {
      if (util.Random.nextFloat() < probMut) {
        // Return a brand new subtree.
        randomTree(depth)
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
*/
 
	/**
	 * subtrees on this tree with subtrees from the other.
	 
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
  }*/

 /**
	* Evaluates this source tree against a list of list containing parameters and their
	* expected output. Returns a score based on the tree's performance.
	*
	* data should be of the form:
	* ((x11,x12,x13...y1),
	* (x21,x22... y2),
	* ...
	* (xn1, xn2... yn))
	
  def scoreAgainstData(data: List[List[AnyVal]]):Int = {
    val scores = for (v <- data) yield score(v)
    (scores.sum / data.length).toInt
  }*/

  /**
	 * Returns absolute differenc between the tree's evaluation
	 * of some parameters and the expected result.
	 
  def score(v: List[AnyVal]):Int= {
    val s = evaluate(v.dropRight(1)).asInstanceOf[Int] - v.last.asInstanceOf[Int]
    if (s>0) s else -s
  }

  def evaluate(paramlist: List[AnyVal]): AnyVal = {
    root.evaluate(paramlist)
  }

  def test(paramlist: List[AnyVal]) {
    printToString(paramlist)
  }*/













