package com.geishatokyo.codegen.util

import com.geishatokyo.smartmerger.Env

/**
 *
 * User: takeshita
 * DateTime: 13/10/18 11:50
 */
object Logger extends Logger {

  var logger = new ConsoleLogger() with EnvCheck

  def debug(message : String, args : Any*) : Unit = logger.debug(message,args:_*)

  def notChange(message : String,args : Any*) : Unit = logger.notChange(message,args:_*)
  def change(message : String,args : Any*) : Unit = logger.change(message,args:_*)
}

trait Logger{

  def debug(message : String, args : Any*) : Unit
  def notChange(message : String,args : Any*) : Unit
  def change(message : String,args : Any*) : Unit
}

class ConsoleLogger extends Aggregate{
  def log(message: String, args: Any*) = {
    if(Env.printDebugLog) {
      println(message.format(args: _*))
    }
  }
}

trait Aggregate extends Logger {

  def log(message : String, args : Any*) : Unit
  def debug(message : String, args : Any*) : Unit = log(message,args :_*)
  def notChange(message : String,args : Any*) : Unit = log(message,args :_*)
  def change(message : String,args : Any*) : Unit = log(message,args :_*)
}

abstract trait EnvCheck extends Logger{
  abstract override def debug(message : String, args : Any*) : Unit = {
    if(Env.printDebugLog) {
      super.debug(message,args :_*)
    }
  }

  abstract override def notChange(message : String,args : Any*) : Unit = {
    if(Env.printNotChangeLog) {
      super.notChange(message,args :_*)
    }
  }
  abstract override def change(message : String,args : Any*) : Unit = {
    if(Env.printChangeLog) {
      super.change(message,args :_*)
    }
  }
}
