package com.geishatokyo.smartmerger.injection

import com.geishatokyo.smartmerger.parse.ParsedData

import scala.util.matching.Regex

/**
 * Created by takeshita on 2015/01/15.
 */
trait Injection {

  def name : String
  def text : String

  def willMerge(parsedData : ParsedData) : Boolean

  def autoIndent : Boolean

}


object Injection{

  case class Always(name : String, text : String,autoIndent : Boolean) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = true
  }
  case class Contain(name : String,text : String,autoIndent : Boolean,checkToContains : String) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = {
      parsedData.rawString.contains(checkToContains)
    }
  }
  case class Match(name : String,text : String,autoIndent : Boolean, checkToMatch : Regex) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = {
      checkToMatch.findFirstIn( parsedData.rawString).isDefined
    }
  }
  abstract class Not( in : Injection) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = !in.willMerge(parsedData)
  }
  case class NotContain(name : String,text : String,autoIndent : Boolean,checkToContains : String) extends Not(Contain(name,text,autoIndent,checkToContains))
  case class NotMatch(name : String,text : String,autoIndent : Boolean, checkToMatch : Regex) extends Not(Match(name,text,autoIndent,checkToMatch))


  def apply(name : String,text : String) : Injection = {
    Always(name,text,true)
  }

  implicit class InjectionOps( a : Injection){
    def ifNotContain(checkToContain : String) = NotContain(a.name,a.text,a.autoIndent,checkToContain)
    def ifNotContainSame = NotContain(a.name,a.text,a.autoIndent,a.text)
    def ifNotMatch(checkToMatch : Regex) = NotMatch(a.name,a.text,a.autoIndent,checkToMatch)
    def ifContain(checkToContain : String) = Contain(a.name,a.text,a.autoIndent,checkToContain)
    def ifMatch(checkToMatch : Regex) = Match(a.name,a.text,a.autoIndent,checkToMatch)
  }


}