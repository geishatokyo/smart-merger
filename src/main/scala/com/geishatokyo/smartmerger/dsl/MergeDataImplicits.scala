package com.geishatokyo.smartmerger.dsl

import com.geishatokyo.smartmerger.parse.ReplaceBlock
import com.geishatokyo.smartmerger.merge.{RegexConditionInsertMBlock, ReplaceMBlock}

/**
 * Created by takeshita on 2014/06/04.
 */
object MergeDataImplicits {


  implicit def nameAndCodeToMergeBlock( v : (String,String)) = {
    ReplaceMBlock(Some(v._1),v._2)
  }

  implicit def codeToMergeBlock( code : String) = {
    ReplaceMBlock(None,code)
  }

  implicit def nameRegexAndCodeToInsertBlock(v : (String,String,String)) = {
    RegexConditionInsertMBlock(Some(v._1),v._2.r,v._3)
  }


}
