package org.stairwaybook.scells

import swing._
import scala.swing.event.ValueChanged

class Model(val height: Int, width: Int) extends Evaluator with Arithmetic {
  
  case class ValueChanged(cell: Cell) extends event.Event

  case class Cell(row: Int, column: Int) extends Publisher {
    private var v: Double = 0
    def value: Double = v
    def value_=(w: Double) {
      if (!(v == w || v.isNaN && w.isNaN)) {
        v = w
        publish(ValueChanged(this))
      }
    }

    private var f: Formula = Empty
    def formula: Formula = f
    def formula_=(f: Formula) {
      for (c <- references(formula)) deafTo(c)
      this.f = f
      for (c <- references(formula)) listenTo(c)
      value = evaluate(f)
    }
    
    reactions += {
      case ValueChanged(_) => value = evaluate(formula)
    }
    
    override def toString = formula match {
      case Textual(s) => s
      case _ => value.toString
    }
  }

  val cells = Array.ofDim[Cell](height, width)

  for (i <- 0 until height; j <- 0 until width)
    cells(i)(j) = new Cell(i, j)

}