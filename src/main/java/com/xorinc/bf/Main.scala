package com.xorinc.bf

import java.io.{Console => _, _}
import scopt.{OptionParser, Zero}

import reflect.{ClassTag, classTag}

import engine._

import language.dynamics

import Default._

class Default[+A](val default: A)

trait LowerPriorityImplicits {
  // Stop AnyRefs from clashing with AnyVals
  implicit def defaultNull[A <: AnyRef]:Default[A] = new Default[A](null.asInstanceOf[A])
}

object Default extends LowerPriorityImplicits {
  implicit object DefaultDouble extends Default[Double](0.0)
  implicit object DefaultFloat extends Default[Float](0.0F)
  implicit object DefaultInt extends Default[Int](0)
  implicit object DefaultLong extends Default[Long](0L)
  implicit object DefaultShort extends Default[Short](0)
  implicit object DefaultByte extends Default[Byte](0)
  implicit object DefaultChar extends Default[Char]('\u0000')
  implicit object DefaultBoolean extends Default[Boolean](false)
  implicit object DefaultUnit extends Default[Unit](())

  def default[A](implicit value: Default[A]): A = value.default
}

sealed trait Options extends Dynamic {
  protected def get(s: String): Option[Any]
  final def applyDynamic[A : ClassTag](s: String)(default: A): A = {
    val clazz = classTag[A].runtimeClass
    get(s).getOrElse(default).asInstanceOf[A]
  }
  final def selectDynamic[A : ClassTag : Default](s: String): A = {
    this.applyDynamic[A](s)(default[A])(classTag[A])
  }
}

object Main {

  private implicit def e2t(e: Engine): (String, Engine) = e.name -> e

  val engines = Map(Interpreter, CCompiler)

  def main(args: Array[String]): Unit = {

    val opts = new Options with Dynamic {

      private val self = this

      private val map = collection.mutable.Map.empty[String, Any]

      override protected def get(s: String): Option[Any] = map.get(s)

      private val parser = new OptionParser[Unit]("brainscala") {
        head("brainscala", "1.0")
        opt[Unit]('e', "exit-status") action ((_, _) => map("exitCode") = true) text "outputs current pointer value as exit status"
        opt[Unit]('v', "verbose") action ((_, _) => map("verbose") = true) text "verbose output"
        opt[Unit]('c', "literal-input") action ((_, _) => map("inAsString") = true) text "take the input as a string, not a file"

        for ((s, e) <- engines)
          e.register(this, cmd(s) action {(_, _) => map("engine") = e}, map(_) = _)

        arg[String]("<script>") maxOccurs 1 action ((x, _) =>
        map("in") =
          if(self.inAsString[Boolean]) new StringReader(x)
          else new FileReader(x)
        )
      }

      parser.parse(args, ()) orElse exit(255)

      if(!map.contains("engine")) map("engine") = Interpreter

    }

    val script = {
      val buffer = new StringBuilder
      val in = new BufferedReader(opts.in[Reader])
      var line: String = null
      do {
        line = in.readLine()
        buffer ++= line + "\n"
      } while (line ne null)
      buffer.toString()
    }

    val exitVal = opts.engine[Engine].apply(script, opts)

    if(opts.exitCode[Boolean]) exit(exitVal)
  }
  // keeping the compiler happy
  def exit(i: Int): Nothing = { System.exit(i); ??? }
}