package com.geishatokyo.smartmerger.parse

import com.geishatokyo.smartmerger.TopLevel

/**
 * Created by takeshita on 2014/06/03.
 */
case class ParsedData(blocks : Block*) {

  lazy val nameToBlock = {
    blocks.collect({
      case t : BlockWithStartTag => t
    }).groupBy(_.name)
  }

  lazy val blockNames : Set[String] = nameToBlock.keys.toSet
  def getBlocks(name : String) = {
    nameToBlock.getOrElse(name,Nil)
  }


  lazy val topLevel = {
    blocks.collect({
      case t : InsertPoint => TopLevel.Replace
      case t : ReplaceBlock => TopLevel.Replace
      case t : HoldBlock => TopLevel.Hold
    }) match{
      case head :: tails => {
        if(tails.forall(_ == head)){
          head
        }else{
          throw new Exception("Replace statement and Hold statement can't exists in same file")
        }
      }
      case Nil => TopLevel.None
    }
  }

  lazy val rawString = blocks.map(_.text).mkString


}
