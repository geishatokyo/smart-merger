package com.geishatokyo.smartmerger.injection

import com.geishatokyo.smartmerger.parse._
import com.geishatokyo.smartmerger.TopLevel

/**
 * Created by takeshita on 2014/06/03.
 */
object Injector{

  def apply() : Injector = Injector(new InjectionRule())
}


case class Injector(mergeRule : InjectionRule) {

  def merge(remainsInsideHold : ParsedData,notRemainsInsideHold : ParsedData) : ParsedData = {
    if(remainsInsideHold.blocks.exists(_.isInstanceOf[SkipMerge])){
      return remainsInsideHold
    }

    if(remainsInsideHold.topLevel == TopLevel.Replace){
      throw new Exception(s"Base file top level must be Hold.But was replace")
    }

    if(notRemainsInsideHold.topLevel == TopLevel.Replace){
      throw new Exception(s"Merge file top level must be Hold.But was replace")
    }

    if(!mergeRule.ignoreNotExistMergeBlock){
      val notExists = notRemainsInsideHold.blockNames.filter(!remainsInsideHold.blockNames.contains(_))
      if(notExists.size > 0){
        throw new Exception(s"${notExists} blocks does not exist in destination file.")
      }
    }

    if(!mergeRule.leftNotMergedBlock){
      val notMerges = remainsInsideHold.blockNames.filter(!notRemainsInsideHold.blockNames.contains(_))
      if(notMerges.size > 0){
        throw new Exception(s"${notMerges} blocks are not merged.")
      }
    }

    val blocks = notRemainsInsideHold.blocks.map(b => b match {
      case b : HoldBlock => {
        remainsInsideHold.getBlocks(b.name) match{
          case Nil => b
          case h :: tails => {
             h
          }
        }
      }
      case b => b
    })
    ParsedData(blocks :_*)
  }

  def merge(parsedData : ParsedData,mergeData : InjectionData) : ParsedData = {
    if(parsedData.blocks.exists(_.isInstanceOf[SkipMerge])){
      return parsedData
    }
    if(parsedData.topLevel == TopLevel.Hold){
      throw new Exception(s"ParsedData top level must be Replace.But was ${parsedData.topLevel}")
    }

    if(!mergeRule.ignoreNotExistMergeBlock){
      val notExists = mergeData.blockNames.filter(!parsedData.blockNames.contains(_))
      if(notExists.size > 0){
        throw new Exception(s"${notExists} blocks does not exist in destination file.")
      }
    }

    if(!mergeRule.leftNotMergedBlock){
      val notMerges = parsedData.blockNames.filter(!parsedData.blockNames.contains(_))
      if(notMerges.size > 0){
        throw new Exception(s"${notMerges} blocks are not merged.")
      }
    }

    def getReplaceText(name : String) = {
      mergeData.getBlocks(name) match{
        case Nil => None
        case list => {
          val replaceTexts = list.flatMap(_ match{
            case cib : ConditionalInjection => {
              if (cib.needInsert(parsedData)) {
                Some(cib.text)
              } else None
            }
            case r : ReplaceInjection => Some(r.text)
          })
          if(replaceTexts.size > 0) Some(replaceTexts.mkString(System.lineSeparator()))
          else None
        }
      }
    }


    val blocks = parsedData.blocks.map(b => b match{
      case b : ReplaceBlock => {
        getReplaceText(b.name) match{
          case Some(t) => b.copyWithTextWithoutTag(t)
          case None => b
        }
      }
      case b : InsertPoint => {
        getReplaceText(b.name) match{
          case Some(t) => b.copyWithTextWithoutTag(t)
          case None => b
        }
      }
      case b : Block => b
    })


    ParsedData(blocks :_*)
  }
}
