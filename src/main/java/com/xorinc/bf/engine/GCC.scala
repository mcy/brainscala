package com.xorinc.bf.engine

import java.io._

import sys.process._

import com.xorinc.bf.Compiler._
import com.xorinc.bf.{Main, Options}
import scopt.{OptionParser, OptionDef}

object GCC extends Engine {

  object JNI extends Engine {

    override val name: String = "jni"

    override def register(parser: OptionParser[Unit], cmd: OptionDef[_, Unit], update: (String, Any) => Unit): Unit = {
      import parser.{cmd => _, _}
      cmd children (
        parser.opt[String]('p', "path") action ((s, _) => update("jni_package", s)) required() maxOccurs 1
          validate(s => if(s.matches(jniPathPattern)) success else failure("path is not of the form `my.package.AnClass.method'"))
          text "path to the native method, in the form `my.package.AnClass.method'",
        parser.opt[File]('d', "directory") action ((f, _) => update("jni_dir", f)) maxOccurs 1
          text "directory to do JNI compilation and output files"
      ) text """output a jni-compatible library, to the given method.
               |if `exit-status' is set, the returned value is int
               |otherwise, it will return void (Unit)""".stripMargin
    }

    override def apply(src: String, opts: Options): Int = {

      val prog =
        try compile(src)
        catch {
          case ParseException(m) => println(m); Main.exit(255)
        }

      val optimized = optimize(prog.ops).map(toC).mkString("\n")

      val width = opts.applyDynamic[String]("width")(sizes(8))

      val path = opts.jni_package[String].split("\\.")
      val dir = {
        val d = opts.jni_dir[File]
        if(d == null) new File("") else d
      }
      val qualName = path.dropRight(1).mkString(".")
      val pkg = path.dropRight(2).mkString(".")

      def write(f: File, s: String) = {
        if(!f.exists()) { f.getParentFile.mkdirs(); f.createNewFile() }
        val out = new BufferedWriter(new FileWriter(f))
        out.append(s)
        out.close()
      }

      def read(f: File) = {
        val buffer = new StringBuilder
        val in = new BufferedReader(new FileReader(f))
        var line: String = null
        do {
          line = in.readLine()
          buffer ++= line + "\n"
        } while (line ne null)
        buffer.toString()
      }

      val className = path(path.length - 2)
      val hasReads = src.contains(",")

      val jWidth = width match {
        case "char" => "byte"
        case "short int" => "short"
        case "long int" => "int"
      }

      val JWidth = jWidth(0).toUpper + jWidth.drop(1)

      val jSrc = jniJavaSrc(path, jWidth, opts.exitCode[Boolean], hasReads)

      write(new File(dir, className + ".java"), jSrc)

      s"javac -d . $className.java" #-> dir !

      s"javah $qualName" #-> dir !

      val hName = qualName.replace('.', '_')

      val header = read(new File(dir, hName + ".h"))

      val sigPattern =
        """
          |JNIEXPORT (.+) JNICALL (.+)
          |  \(JNIEnv \*, jclass(, jbyteArray)?\);
          |""".stripMargin.r

      val sig = sigPattern.findFirstMatchIn(header).get

      val cSrc =
        s"""
         |#include <jni.h>
         |#include "${hName + ".h"}"
         |${ if(hasReads) "\n#define HASREADS\n" else "" }
         |JNIEXPORT ${sig.group(1)} JNICALL ${sig.group(2)} (JNIEnv *env, jclass clazz
         |#ifdef HASREADS
         |  ,j${jWidth}Array a
         |#endif
         |) {
         |  $width tape[${1 << 16}] = {0};
         |  $width *p = tape;
         |  char c = 0;
         |#ifdef HASREADS
         |  jsize index = 0;
         |  j$jWidth *in = (*env)->Get${JWidth}ArrayElements(env, a, NULL);
         |  jsize length;
         |  if (NULL == in)
         |    length = 0;
         |  else
         |    length = (*env)->GetArrayLength(env, a);
         |#define getchar() (index < length ? *(in++) : '\0'); index++;
         |#endif
         |  /* start generated code */
         |  $optimized
         |  /* end generated code */
         |  ${if(opts.exitCode[Boolean]) "return (int) *p;" else ""}
         |}
       """.stripMargin

      val cFile = path.mkString("_") + ".c"

      write(new File(dir, cFile), cSrc)

      val sep = File.separator

      val includes = new File(new File(System.getProperty("java.home")).getParentFile, "include")

      val iincludes = includes.listFiles().filter(_.isDirectory).map("-I" + _.getCanonicalPath + "").mkString(" ")

      val i = s"gcc -w -I${includes.getCanonicalPath} $iincludes -dynamiclib -o ${System.mapLibraryName(path.mkString("_"))} $cFile" #-> dir !

      if(i != 0) {
        println("It appears there is some sort of error generating the shared library.")
        println("This is probably because I wasn't able to find the jni headers.")
      }

      i
    }
  }
  
  override val name: String = "gcc"

  private val sizes = Map (
    8 -> "char",
    16 -> "short int",
    32 -> "long int"
  )

  override def register(parser: OptionParser[Unit], cmd: OptionDef[_, Unit], update: (String, Any) => Unit): Unit = {
    import parser.{cmd => _, _}
    cmd children (
      /*parser.opt[Int]("datawidth") abbr "dw" action ((i, _) => update("width", sizes(i))) maxOccurs 1
        validate (i => if (sizes.contains(i)) success else failure(s"size must be one of ${sizes.keys.mkString(", ")}"))
        text "width of the data type to use for the pointer",*/
      parser.opt[String]('o', "out") action ((s, _) => update("out", s)) required() maxOccurs 1
        text "file to output executable to"
    ) text "compiles entered script into native code via a C/gcc intermediary"
  }

  private sealed trait Instr
  private case class WrappedInstr(count: Int, i: Instruction) extends Instr
  private case class WrappedLoop(l: List[Instr]) extends Instr

  private def optimize(l: List[Instruction]): List[Instr] = {
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
  
  override def apply(src: String, opts: Options): Int = {

    val prog =
      try compile(src)
      catch {
        case ParseException(m) => println(m); Main.exit(255)
      }

    val optimized = optimize(prog.ops).map(toC).mkString("\n")

    val width = opts.applyDynamic[String]("width")(sizes(8))
  
    val cSrc =
      s"""
      |int main (void) {
      |$width tape[${1 << 16}] = {0};
      |$width *p = tape;
      |char c = 0;
      |$optimized
      |return ${if(opts.exitCode[Boolean]) "(int) *p" else "0"};
      |}
    """.stripMargin//.replace("\n", "\\n")

    s"echo $cSrc" #| s"gcc -w -x c -o ${opts.out[String]} -" !
    
  }

  private def toC(i: Instr): String = i match {
    case WrappedInstr(i, op) => op match {
      case Plus  => s"*p += $i;"
      case Minus => s"*p -= $i;"
      case Right => s"p += $i;"
      case Left  => s"p -= $i;"
      case Write => "c = (char) *p; " + "putchar(c); " * i
      case Read  => "*p = getchar(); " * i
    }
    case WrappedLoop(ops) => s"while(*p){\n${ops.map(toC).mkString("\n")}\n}"
  }

  private val jniPathPattern = "(?:[a-zA-Z_0-9]+\\.)+(?:[a-zA-Z_0-9]+)"

  private def jniJavaSrc(path: Array[String], width: String, hasReturn: Boolean, hasInput: Boolean) = {
    val pack =
      if(path.length > 2)
        s"package ${path.slice(0, path.length - 2).mkString(".")};"
      else ""
    val name = path(path.length - 2)
    val meth = path(path.length - 1)
    val arrType =
      if(hasInput)
        s"$width[] a"
      else ""
    val ret = if(hasReturn) "int" else "void"



    s"""
       |$pack
       |public final class $name {
       |    static { System.loadLibrary("${path.mkString("_")}"); }
       |    public $name() { throw new Error(); }
       |    public static native $ret $meth($arrType);
       |    public static void main(String... args) { $meth(${if(arrType.nonEmpty) s"new $width[0]" else ""}); }
       |}
     """.stripMargin
  }

  private implicit class _procFile(val s: String) extends AnyVal {
    def #->(f: File): ProcessBuilder = Process(s, f)
  }

}
