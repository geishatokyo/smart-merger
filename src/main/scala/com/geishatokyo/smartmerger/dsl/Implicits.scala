package com.geishatokyo.smartmerger.dsl

import com.geishatokyo.smartmerger.parse.ReplaceBlock
import com.geishatokyo.smartmerger.injection.{ContainsConditionInjection, RegexConditionInjection, ReplaceInjection}
import scala.util.matching.Regex

/**
 * Created by takeshita on 2014/06/04.
 */
object Implicits {

  implicit class InjectionWrapper(code : String){

    def to(blockName : String) = {
      BlockNameAndCode(blockName,code)
    }


  }

  case class BlockNameAndCode(blockName : String, code : String){

    def ifNotContains( str : String) = {
      ContainsConditionInjection(Some(blockName),str,code)
    }

    def ifNot(r : Regex) = {
      RegexConditionInjection(Some(blockName),r,code)
    }

  }
  implicit def toInjectionBlock( b: BlockNameAndCode) = {
    ReplaceInjection(Some(b.blockName),b.code)
  }

  def replace(s: String) = {
    InjectionWrapper(s)
  }


  implicit def nameAndCodeToMergeBlock( v : (String,String)) = {
    ReplaceInjection(Some(v._1),v._2)
  }

  implicit def codeToMergeBlock( code : String) = {
    ReplaceInjection(None,code)
  }

  implicit def nameRegexAndCodeToInsertBlock(v : (String,String,String)) = {
    RegexConditionInjection(Some(v._1),v._2.r,v._3)
  }


}
