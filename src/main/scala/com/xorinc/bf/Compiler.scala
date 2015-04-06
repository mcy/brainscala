package com.xorinc.bf

object Compiler {

  case class ParseException(message: String) extends Exception(message)

  private val legals = "+-<>.,[]".toCharArray.toSet
  private val nonLoopChars = legals - '[' - ']'

  private object nonLoop { def unapply(c: Char): Option[Char] = if(nonLoopChars(c)) Some(c) else None }
  private def instruction(c: Char): Instruction = c match {
    case '+' => Plus
    case '-' => Minus
    case '>' => Right
    case '<' => Left
    case '.' => Write
    case ',' => Read
  }

  sealed trait                Instruction
  case   object Plus  extends Instruction
  case   object Minus extends Instruction
  case   object Right extends Instruction
  case   object Left  extends Instruction
  case   object Write extends Instruction
  case   object Read  extends Instruction

  final case class Loop(ops: List[Instruction]) extends Instruction

  case class Program(ops: List[Instruction])

  def compile(s: String) = {
    if (
      s.iterator.filter(s => s == '[' || s == ']').map {
        case '[' => 1
        case ']' => -1
      }.sum != 0
    ) throw ParseException("Unbalanced loop delimiters!")

    def doCompile(iter: Iterator[Char], list: List[Instruction] = Nil): Program = {
      def enterLoop(list: List[Instruction] = Nil): Loop = {
        enterLoop(
          (iter.next() match {
            case nonLoop(c) => instruction(c)
            case '[' => enterLoop()
            case ']' => return Loop(list.reverse)
          }) :: list
        )
      }
      if (iter.hasNext)
        doCompile(iter,
          (iter.next() match {
            case nonLoop(c) => instruction(c)
            case '[' => enterLoop()
          }) :: list
        )
      else Program(list.reverse)
    }
    doCompile(s.iterator.filter(legals(_)))
  }

}
