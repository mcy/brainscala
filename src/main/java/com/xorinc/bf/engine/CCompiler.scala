package com.xorinc.bf.engine

import java.io.File

import com.xorinc.bf.Compiler._
import com.xorinc.bf.{Main, Options}
import scopt.{OptionParser, OptionDef}

object CCompiler extends Engine {
  override val name: String = "gcc"

  private val sizes = Map (
    8 -> "char",
    16 -> "short int",
    32 -> "long int"
  )

  override def register(parser: OptionParser[Unit], cmd: OptionDef[_, Unit], update: (String, Any) => Unit): Unit = {
    import parser.{cmd => _, _}
    cmd children (
      parser.opt[Int]("datawidth") abbr "dw" action ((i, _) => update("width", sizes(i))) maxOccurs 1
        validate (i => if (sizes.contains(i)) success else failure(s"size must be one of ${sizes.keys.mkString(", ")}"))
        text "width of the data type to use for the pointer",
      parser.opt[String]('o', "out") action ((s, _) => update("out", s)) required() maxOccurs 1
        text "file to output executable to"
    )
  }

  private sealed trait Instr
  private case class WrappedInstr(count: Int, i: Instruction) extends Instr
  private case class WrappedLoop(l: List[Instr]) extends Instr

  override def apply(src: String, opts: Options): Int = {

    val prog =
      try compile(src)
      catch {
        case ParseException(m) => println(m); Main.exit(255)
      }

    def optimize(l: List[Instruction]): List[Instr] = {
      def split(list: List[Instruction]) : List[(Int, Instruction)] = list match {
        case Nil => Nil
        case h::t =>
          val segment = list takeWhile {h eq}
          val len = segment.length
          (len, h) :: split(list drop len)
      }
      split(l) map {
        case (_, Loop(ops)) => WrappedLoop(optimize(ops))
        case (n, i) => WrappedInstr(n, i)
      }
    }

    val optimized = optimize(prog.ops).map(toC).mkString("\n")

    val width = opts.applyDynamic[String]("width")(sizes(8))
    val cSrc =
      s"""
        |int main (void) {
        |$width tape[${1 << 16}] = {0};
        |$width *p = tape;
        |$optimized
        |return ${if(opts.exitCode[Boolean]) "(int) *p" else "0"};
        |}
      """.stripMargin//.replace("\n", "\\n")

    import sys.process._

    val compiler = Process(s"echo ${cSrc}") #| Process(s"gcc -w -x c -o ${opts.out[String]} -")

    compiler!
  }

  private def toC(i: Instr): String = i match {
    case WrappedInstr(i, op) => op match {
      case Plus  => s"*p+=$i;"
      case Minus => s"*p-=$i;"
      case Right => s"p+=$i;"
      case Left  => s"p-=$i;"
      case Write => "putchar(*p); " * i
      case Read  => "*p=getchar(); " * i
    }
    case WrappedLoop(ops) => s"while(*p){\n${ops.map(toC).mkString("\n")}\n}"
  }

}
