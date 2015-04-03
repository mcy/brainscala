package com.xorinc.bf.engine

import com.xorinc.bf.Options
import scopt.OptionDef

abstract class Engine extends ((String, Options) => Int) {

  val name: String

  def register(cmd: OptionDef[_, _]): Unit = {}
}
