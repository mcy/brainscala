package com.xorinc.bf.engine

import com.xorinc.bf.Options
import scopt.{OptionParser, OptionDef}

abstract class Engine extends ((String, Options) => Int) {

  val name: String

  def register(parser: OptionParser[Unit], cmd: OptionDef[_, Unit], update: (String, Any) => Unit): Unit = {}
}
