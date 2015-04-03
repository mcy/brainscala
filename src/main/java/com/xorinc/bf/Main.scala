package com.xorinc.bf

import java.io.{Console => _, _}
import scopt.{OptionParser, Zero}

import reflect.{ClassTag, classTag}

import engine._

import language.dynamics
sealed trait Options extends Dynamic {
  protected def get(s: String): Option[Any]
  final def selectDynamic[A : ClassTag](s: String): A = {
    val clazz = classTag[A].runtimeClass
    clazz.cast(get(s).getOrElse(default[A])).asInstanceOf[A]
  }

  private sealed case class Def[A](x: A)
  private object default {

    def default[A](implicit ev: Def[A]): A = ev.x

    implicit val Z = Def[Byte](0.toByte)
    implicit val B = Def[Byte](0.toByte)
    implicit val S = Def[Short](0.toShort)
    implicit val I = Def[Int](0)
    implicit val J = Def[Long](0L)
    implicit val F = Def[Float](0F)
    implicit val D = Def[Double](0D)
    implicit val L = Def[AnyRef](null)
  }
}

object Main {

  private implicit def e2t(e: Engine): (String, Engine) = e.name -> e

  val engines = Map(Interpreter)

  def main(args: Array[String]): Unit = {

    val opts = new Options with Dynamic { self: this.type =>

      private val map = collection.mutable.Map.empty[String, Any]

      override protected def get(s: String): Option[Any] = map.get(s)

      private val parser = new OptionParser[Unit]("brainscala") {
        head("brainscala", "1.0")
        opt[Unit]('e', "exit-status") action ((_, _) => map("exitCode") = true) text "outputs current pointer value as exit status"
        opt[Unit]('v', "verbose") action ((_, _) => map("verbose") = true) text "verbose output"
        opt[Unit]('c', "literal-input") action ((_, _) => map("inAsString") = true) text "take the input as a string, not a file"

        for ((s, e) <- engines)
          e.register(cmd(s) action {(_, _) => map("engine") = e})

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