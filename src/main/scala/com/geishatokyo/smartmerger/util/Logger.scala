package com.geishatokyo.codegen.util

/**
 *
 * User: takeshita
 * DateTime: 13/10/18 11:50
 */
object Logger extends Logger {

  var logger = new ConsoleLogger()

  def log(message: String, args: Any*) = logger.log(message,args:_*)
}

trait Logger{

  def log(message : String, args : Any*) : Unit

}

class ConsoleLogger extends Logger{
  def log(message: String, args: Any*) = {
    println(message.format(args :_*))
  }
}