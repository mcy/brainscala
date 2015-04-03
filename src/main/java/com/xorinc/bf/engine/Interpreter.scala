package com.xorinc.bf.engine

import java.io.{Reader, BufferedReader}

import com.xorinc.bf._
import Compiler._

case object Interpreter extends Engine {

  override def apply(src: String, opts: Options): Int = {

    val prog =
      try compile(src)
      catch {
        case ParseException(m) => println(m); Main.exit(255)
      }

    run(prog)
  }

  def run(p: Program): Int = {
    p.ops.foreach(run); Tape()
  }

  def run(i: Instruction): Unit = i match {
    case Plus  => Tape.plus ()
    case Minus => Tape.minus()
    case Right => Tape.right()
    case Left  => Tape.left ()
    case Write => Console.out.print(Tape())
    case Read =>
      Tape() =
        (Console.in.read() match {
          case -1 => 0
          case c => c
        }).toChar
    case Loop(ops) => do ops.foreach(run) while (Tape() != 0)
  }

  object Tape {

    private val default = '\0'
    private val positive = collection.mutable.ArrayBuffer.fill(64)(default)
    private val negative = collection.mutable.ArrayBuffer.fill(63)(default)
    private var _pointer = 0

    def pointer: Int = _pointer

    private def updateLengths(): Unit =
      if (pointer > 0 && pointer >= positive.length) positive += default
      else if (pointer < 0 && -pointer >= negative.length) negative += default

    def right(): Unit = {
      _pointer += 1
      updateLengths()
    }

    def left(): Unit = {
      _pointer -= 1
      updateLengths()
    }

    def plus(): Unit = {
      if (pointer >= 0)
        positive(pointer) = (positive(pointer) + 1).toChar
      else
        negative(-pointer) = (negative(-pointer) + 1).toChar
    }

    def minus(): Unit = {
      if (pointer >= 0)
        positive(pointer) = (positive(pointer) - 1).toChar
      else
        negative(-pointer) = (negative(-pointer) - 1).toChar
    }

    def apply(): Char =
      if (pointer >= 0)
        positive(pointer)
      else
        negative(-pointer)

    def update(c: Char): Unit =
      if (pointer >= 0)
        positive(pointer) = c
      else
        negative(-pointer) = c
  }

  override val name: String = "interpret"
}
