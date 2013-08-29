import com.scalatrees.stree._

object treeTests {
  def main(args: Array[String]) {

  /**
   * Some common functions wrapped in tfuncs for testing.
   **/
  val tfunc_add=new Tfunc("add", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] + paramlist(1).asInstanceOf[Int])
  val tfunc_sub=new Tfunc("sub", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] -  paramlist(1).asInstanceOf[Int])
  val tfunc_mul=new Tfunc("mul", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] * paramlist(1).asInstanceOf[Int])
  //val tfunc_pow=new Tfunc("pow", 2, (paramlist: List[Any])=>scala.math.pow(paramlist(0).asInstanceOf[Int],paramlist(1)).asInstanceOf[Int])
   val tfunc_rand=new Tfunc("rand", 1, (paramlist: List[Any])=>rand_base(paramlist))
   val tfunc_mod=new Tfunc("mod", 2, (paramlist: List[Any])=>paramlist(0).asInstanceOf[Int] % paramlist(1).asInstanceOf[Int])

  val tfunc_if=new Tfunc("if",3,(paramlist: List[Any])=>if_base(paramlist))
  val tfunc_gt=new Tfunc("gt",2,(paramlist: List[Any])=>is_greater_base(paramlist))

  val flist = List[Tfunc](tfunc_if,tfunc_gt,tfunc_add,tfunc_sub, tfunc_mul)
  val pars = List[Any](1,3,2)

  val t = new Stree(3,flist,12,0.5f,0.5f)

  t.printToString(pars)

  }

/**
 * Nice test tree.
 */
def maketree() {

}
/**
 * The common ternary if statement:
 * if (p(1)) return p(1)
 * else return p(2)
 */
def if_base(paramlist: List[Any]): Any = {
  if (paramlist(0).asInstanceOf[Int] > 0) {
    paramlist(1)
  } else {
    paramlist(2)
  }
}

/**
 * GT operation returning true=1, false=0
 */
def is_greater_base(paramlist: List[Any]): Any = {
  if (paramlist(0).asInstanceOf[Int] > paramlist(1).asInstanceOf[Int]) {
    return 1
  } else {
    return 0
  }
}

def abs_base(paramlist: List[Any]): Any = {
  val x = paramlist(0).asInstanceOf[Int]
  if (x>0) x else -x
}

def min_base(paramlist: List[Any]): Any = {
  val x = paramlist(0).asInstanceOf[Int]
  val y = paramlist(0).asInstanceOf[Int]
  if (x>=y) x else y
}

def rand_base(paramlist: List[Any]): Any = {
  val x = abs_base(paramlist)
  util.Random.nextInt(min_base(List[Any](x,1)).asInstanceOf[Int])

}

}


