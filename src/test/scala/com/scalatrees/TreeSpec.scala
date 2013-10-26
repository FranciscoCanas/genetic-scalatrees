package com.scalatrees
import org.specs2._

class TreeSpecextends extends Specification { def is =                          s2"""
  Specifications to validate NodeTree
  
  Functions nodes should [Integer]
    be able to add two constant nodes                                              $add1
    be able to add a constant node with a function node                            $add2
    be able to add a function node with a constant node                            $add3
    be able to add a function node with a function node                            $add4
    
  Functions nodes should [Double]
    be able to add two constant nodes                                              $add5
    be able to add a constant node with a function node                            $add6
    be able to add a function node with a constant node                            $add7
    be able to add a function node with a function node                            $add8
    
                                                                                  """
  val FNode2 = FNode(Add,List(CNode(1), CNode(1)))
  val FNodeDouble22 = FNode(Add,List(CNode(1.1), CNode(1.1)))
  def add1 = FNode2.evaluate === 2
  def add2 = FNode(Add,List(CNode(1), FNode2)).evaluate === 3
  def add3 = FNode(Add,List(FNode2, CNode(1))).evaluate === 3
  def add4 = FNode(Add, List(FNode2, FNode2)).evaluate === 4
  
  def add5 = FNodeDouble22.evaluate === 2.2
  def add6 = FNode(Add,List(CNode(1), FNodeDouble22)).evaluate === 3.2
  def add7 = FNode(Add,List(FNodeDouble22, CNode(1))).evaluate === 3.2
  def add8 = FNode(Add, List(FNodeDouble22, FNodeDouble22)).evaluate === 4.4
}