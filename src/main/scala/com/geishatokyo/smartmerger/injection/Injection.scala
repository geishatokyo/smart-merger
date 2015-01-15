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

}


object Injection{

  case class Always(name : String, text : String) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = true
  }
  case class Contain(name : String,text : String,checkToContains : String) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = {
      parsedData.rawString.contains(checkToContains)
    }
  }
  case class Match(name : String,text : String, checkToMatch : Regex) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = {
      checkToMatch.findFirstIn( parsedData.rawString).isDefined
    }
  }
  abstract class Not( in : Injection) extends Injection{
    override def willMerge(parsedData: ParsedData): Boolean = !in.willMerge(parsedData)
  }
  case class NotContain(name : String,text : String,checkToContains : String) extends Not(Contain(name,text,checkToContains))
  case class NotMatch(name : String,text : String, checkToMatch : Regex) extends Not(Match(name,text,checkToMatch))


  def apply(name : String,text : String) : Injection = {
    Always(name,text)
  }

  implicit class InjectionOps( a : Injection){
    def ifNotContain(checkToContain : String) = NotContain(a.name,a.text,checkToContain)
    def ifNotContainSame = NotContain(a.name,a.text,a.text)
    def ifNotMatch(checkToMatch : Regex) = NotMatch(a.name,a.text,checkToMatch)
    def ifContain(checkToContain : String) = Contain(a.name,a.text,checkToContain)
    def ifMatch(checkToMatch : Regex) = Match(a.name,a.text,checkToMatch)
  }


}