package com.xorinc.bf

import java.io.{Console => _, _}

object Main {

  def exit(i: Int): Nothing = {
    System.exit(i); ??? // keeping the compiler happy
  }

  def main(args: Array[String]): Unit = {
    case class Conf(in: Reader = null, inAsString: Boolean = false, exitCode: Boolean = false, verbose: Boolean = false)
    val parser = new scopt.OptionParser[Conf]("brainscala") {
      head("brainscala", "1.0")
      opt[Unit]('e', "exit-status") action ((_, c) => c.copy(exitCode = true)) text "outputs current pointer value as exit status"
      opt[Unit]('v', "verbose") action ((_, c) => c.copy(verbose = true)) text "verbose output"
      opt[Unit]('c', "literal-input") action ((_, c) => c.copy(inAsString = true)) text "take the input as a string, not a file"
      arg[String]("<script>") maxOccurs 1 action ((x, c) => c.copy(in =
        if(c.inAsString) new StringReader(x)
        else new FileReader(x)
      ))
    }
    val opts = parser.parse(args.toSeq, Conf()) getOrElse { exit(255) }

    val script = {
      val buffer = new StringBuilder
      val in = new BufferedReader(opts.in)
      var line: String = null
      do {
        line = in.readLine()
        buffer ++= line + "\n"
      } while (line ne null)
      buffer.toString()
    }

    val prog =
      try Interpreter.compile(script)
      catch {
        case Interpreter.ParseException(m) => println(m); exit(255)
      }

    val exitVal = prog.run()

    if(opts.exitCode) exit(exitVal)
  }
}

object Interpreter {

  case class ParseException(message: String) extends Exception(message)

  val legals = "+-<>.,[]".toCharArray.toSet

  sealed trait Instruction {

    def apply(): Unit
  }
  case object Plus extends Instruction {

    override def apply(): Unit = Tape.plus()
  }
  case object Minus extends Instruction {

    override def apply(): Unit = Tape.minus()
  }
  case object Right extends Instruction {

    override def apply(): Unit = Tape.right()
  }
  case object Left extends Instruction {

    override def apply(): Unit = Tape.left()
  }
  case object Write extends Instruction {

    override def apply(): Unit = Console.out.print(Tape())
  }
  case object Read extends Instruction {

    override def apply(): Unit = Tape() = Console.in.read().toChar
  }
  sealed case class Loop(ops: List[Instruction]) extends Instruction {

    override def apply(): Unit =
      do ops.foreach(_())
      while (Tape() != 0)
  }

  case class Program(ops: List[Instruction]) {

    def run(): Int = {
      ops.foreach(_())
      Tape()
    }
  }

  def compile(s: String) = {
    if(
      s.iterator.filter(s => s == '[' || s == ']').map {
        case '[' => 1
        case ']' => -1
      }.sum != 0
    ) throw new Exception() // TODO

    def doCompile(iter: Iterator[Char], list: List[Instruction] = Nil): Program = {
      def enterLoop(list: List[Instruction] = Nil): Loop = {
          enterLoop(
            (iter.next() match {
              case '+' => Plus
              case '-' => Minus
              case '>' => Right
              case '<' => Left
              case '.' => Write
              case ',' => Read
              case '[' => enterLoop()
              case ']' => return Loop(list.reverse)
            }) :: list
          )
      }
      if (iter.hasNext)
        doCompile(iter,
          (iter.next() match {
            case '+' => Plus
            case '-' => Minus
            case '>' => Right
            case '<' => Left
            case '.' => Write
            case ',' => Read
            case '[' => enterLoop()
          }) :: list
        )
      else Program(list.reverse)
    }
    doCompile(s.iterator.filter(legals(_)))
  }
}

object Tape {

  import collection.mutable.ArrayBuffer

  private val default = '\0'

  private val positive = ArrayBuffer.fill(64)(default)
  private val negative = ArrayBuffer.fill(63)(default)

  private var _pointer = 0
  def pointer: Int = _pointer

  private def updateLengths(): Unit =
    if(pointer > 0 && pointer > positive.length) positive += default
    else if(pointer < 0 && -pointer > negative.length) negative += default

  def right(): Unit = {
    _pointer += 1
    updateLengths()
  }
  def left(): Unit = {
    _pointer -= 1
    updateLengths()
  }

  def plus(): Unit = {
    if(pointer >= 0)
      positive(pointer) = (positive(pointer) + 1).toChar
    else
      negative(-pointer) = (negative(-pointer) + 1).toChar
  }

  def minus(): Unit = {
    if(pointer >= 0)
      positive(pointer) = (positive(pointer) - 1).toChar
    else
      negative(-pointer) = (negative(-pointer) - 1).toChar
  }

  def apply(): Char =
    if(pointer >= 0)
      positive(pointer)
    else
      negative(-pointer)

  def update(c: Char): Unit =
    if(pointer >= 0)
      positive(pointer) = c
    else
      negative(-pointer) = c
}