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
          text "directory to do JNI compilation and output files",
        parser.opt[Unit]("use-in-stream") action ((_, _) => update("jni_useInput", true)) maxOccurs 1
          text "if the method takes input, it will take an InputStream as an argument",
        parser.opt[Unit]("use-out-stream") action ((_, _) => update("jni_useOutput", true)) maxOccurs 1
          text "the method will take an OutputStream as an argument for piping output into",
        parser.opt[Unit]("use-getchar") action ((_, _) => update("jni_usegetchar", true)) maxOccurs 1
          text "!!DON'T USE ME, I'M BROKEN!! if the method takes input, it will use the native getchar() method"
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
      val useInStr = opts.jni_useInput[Boolean]
      val useOutStr = opts.jni_useOutput[Boolean]
      val useGetChar = opts.jni_usegetchar[Boolean]

      val jWidth = width match {
        case "char" => "byte"
        case "short int" => "short"
        case "long int" => "int"
      }

      val JWidth = jWidth(0).toUpper + jWidth.drop(1)

      val jSrc = jniJavaSrc(path, jWidth, opts.exitCode[Boolean], hasReads && !useGetChar, useInStr && !useGetChar, useOutStr)

      write(new File(dir, className + ".java"), jSrc)

      s"javac -d . $className.java" #-> dir !

      s"javah $qualName" #-> dir !

      val hName = qualName.replace('.', '_')

      val header = read(new File(dir, hName + ".h"))

      val sigPattern =
        """
          |JNIEXPORT (.+) JNICALL (.+)
          |  \(JNIEnv \*, jclass(?:, (.+))?\);
          |""".stripMargin.r

      val sig = sigPattern.findFirstMatchIn(header).get

      val cSrc =
        s"""
         |#include <jni.h>
         |#include "${hName + ".h"}"
         |${ if(useGetChar) "" else if(useInStr) "\n#define INPSTR\n" else if(hasReads) "\n#define HASREADS\n" else "" }
         |${ if(useOutStr) "\n#define OUTSTR\n"}
         |${ if(sig.group(3) ne null) "\n#define EXTRAARG\n" else ""}
         |JNIEXPORT ${sig.group(1)} JNICALL ${sig.group(2)} (JNIEnv *env, jclass clazz
         |#ifdef INPSTR
         |  ,jobject a /* java.io.InputStream */
         |#endif
         |#ifdef HASREADS
         |  ,j${width}array a
         |#endif
         |#ifdef OUTSTR
         |  ,jobject b /* java.io.OutputStream */
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
         |#ifdef INPSTR
         |  jclass InputStream = (*env)->FindClass(env, "java/io/InputStream");
         |  jmethodID read = (*env)->GetMethodID(env, InputStream, "read", "()I");
         |  jint readHolder;
         |#define getchar() (readHolder = (*env)->CallIntMethod(env, a, read), readHolder == -1 ? 0 : readHolder)
         |#endif
         |#ifdef OUTSTR
         |  jclass OutputStream = (*env)->FindClass(env, "java/io/OutputStream");
         |  jmethodID write = (*env)->GetMethodID(env, OutputStream, "write", "(I)V");
         |#define putchar(C) (*env)->CallVoidMethod(env, b, write, C)
         |#endif
         |  /* start generated code */
         |$optimized
         |  /* end generated code */
         |  ${if(opts.exitCode[Boolean]) "return (int) *p;" else ""}
         |}
         |""".stripMargin

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

    private val jniPathPattern = "(?:[a-zA-Z_0-9]+\\.)+(?:[a-zA-Z_0-9]+)"

    private def jniJavaSrc(
      path: Array[String],
      width: String,
      hasReturn: Boolean,
      hasInput: Boolean,
      useInStr: Boolean,
      useOutStr: Boolean
    ) = {
      val pack =
        if(path.length > 2)
          s"package ${path.slice(0, path.length - 2).mkString(".")};"
        else ""
      val name = path(path.length - 2)
      val meth = path(path.length - 1)
      val params = Seq(
        if(useInStr)
          "InputStream a"
        else if(hasInput)
          s"$width[] a"
        else "",
        if(useOutStr)
          "OutputStream b"
        else ""
      ).filter(_.nonEmpty).mkString(", ")
      val args = Seq(
        if(useInStr)
          "System.in"
        else if(hasInput)
          s"new $width[0]"
        else "",
        if(useOutStr)
          "System.out"
        else ""
      ).filter(_.nonEmpty).mkString(", ")
      val ret = if(hasReturn) "int" else "void"
      val imp = if(useInStr || useOutStr) "import java.io.*;" else ""

      s"""
       |$pack
       |$imp
       |public final class $name {
       |    static { System.loadLibrary("${path.mkString("_")}"); }
       |
       |    public static native $ret $meth($params);
       |    public static void main(String... args) { $meth($args); }
       |}
     """.stripMargin
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

  private implicit class _procFile(val s: String) extends AnyVal {
    def #->(f: File): ProcessBuilder = Process(s, f)
  }

}
