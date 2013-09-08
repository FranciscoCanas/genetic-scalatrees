package com.scalatrees

/**
 * Wrapper for a function that holds number of parameters
 * and name of the function as well as the function object.
 * 
 * TODO: GEBERALIZE THIS!!!!!!
 */

//abstract class Tfunc[+A <: AnyVal] {
abstract class Tfunc {
  val name: String
 	val numParam: Int
 	val function: List[AnyVal] => AnyVal
 	/**
 	 * Below are many failed attempts at generalization
 	 * All things in comments of '//' are failed attempts at generalization
 	 * May want to read...
 	 * http://www.chuusai.com/2012/04/27/shapeless-polymorphic-function-values-1/
 	 * http://www.chuusai.com/2012/05/10/shapeless-polymorphic-function-values-2/
 	 * http://stackoverflow.com/questions/663254/scala-covariance-contravariance-question
 	 * http://docs.scala-lang.org/tutorials/tour/variances.html
 	 * http://stackoverflow.com/questions/13321921/covariance-and-contravariance-in-scala-java/13322048#13322048
 	 * https://github.com/milessabin/shapeless
 	 * 
 	 * 
			 	def apply[A](f: A => A, a: A): A = f(a)
			 	def function[B >: A](params: List[B]): B
			 	val function: [B >: A] List[B] => B
			 	Scala values are monomorphic ..... doesn't seeem possible.....
			 	val function: List[A] => A
			 	(x: Int) => x + 1
			 	def cons[B >: A](v: B): List[B]
 	 * 
 	 */
}

//class Function[A <: AnyVal](val name: String, val numParam:Int, val ffs: List[A] => A) extends Tfunc[A]// {
  //def function[Int >: Int](params: List[Int]): Int = params(0) + params(1)
//}
class Function(val name: String, val numParam:Int, val function: List[AnyVal] => AnyVal) extends Tfunc
 


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
    new Function("Add", numParam, (params: List[AnyVal])=>params(0).asInstanceOf[Int] + params(1).asInstanceOf[Int])
  	//new Function("Add", numParam, (params: List[Int])=>params(0) + params(1))
  
  def sub(numParam: Int) =
    new Function("Sub", numParam, (params: List[AnyVal])=>params(0).asInstanceOf[Int] - params(1).asInstanceOf[Int])
  	//new Function("Sub", numParam, (params: List[Int])=>params(0) - params(1))

  def mul(numParam: Int) =
  	new Function("Sub", numParam, (params: List[AnyVal])=>params(0).asInstanceOf[Int] * params(1).asInstanceOf[Int])
    //new Function("Mul", numParam, (params: List[Int])=>params(0) * params(1))
  
  def pow(numParam: Int) =
    new Function("Pow", numParam, (params: List[AnyVal])=>scala.math.pow(params(0).asInstanceOf[Double], params(1).asInstanceOf[Double]))
  	//new Function("Pow", numParam, (params: List[Double]) => scala.math.pow(params(0), params(1)))
  
  def rand(numParam: Int) =
    new Function("Rand", numParam, (paramlist: List[AnyVal])=>rand_base(paramlist.asInstanceOf[List[Int]]))
    //new Function("Rand", numParam, (paramlist: List[Int])=>rand_base(paramlist))
  
  def mod(numParam: Int) =
    new Function("Mod", numParam, (paramlist: List[AnyVal])=>paramlist(0).asInstanceOf[Int] % paramlist(1).asInstanceOf[Int])
  	//new Function("Mod", numParam, (paramlist: List[Int])=>paramlist(0) % paramlist(1))
  
  def iff(numParam: Int) =
    new Function("Iff", numParam, (paramlist: List[AnyVal])=>if_base(paramlist.asInstanceOf[List[AnyVal]]))
  	//new Function("Iff", numParam, (paramlist: List[AnyVal])=>if_base(paramlist))
  
  def gtr(numParam: Int) =
    new Function("Gtr", numParam, (paramlist: List[AnyVal])=>is_greater_base(paramlist.asInstanceOf[List[AnyVal]]))
    //new Function("Gtr", numParam, (paramlist: List[AnyVal])=>is_greater_base(paramlist))
}