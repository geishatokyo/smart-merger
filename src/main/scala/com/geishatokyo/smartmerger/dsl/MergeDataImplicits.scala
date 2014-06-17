package com.geishatokyo.smartmerger.dsl

import com.geishatokyo.smartmerger.parse.ReplaceBlock
import com.geishatokyo.smartmerger.injection.{RegexConditionInjection, ReplaceInjection}

/**
 * Created by takeshita on 2014/06/04.
 */
object MergeDataImplicits {


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
