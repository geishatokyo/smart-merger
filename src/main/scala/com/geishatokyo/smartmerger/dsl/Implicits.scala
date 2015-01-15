package com.geishatokyo.smartmerger.dsl

import com.geishatokyo.smartmerger.injection.Injection
import com.geishatokyo.smartmerger.parse.ReplaceBlock
import scala.util.matching.Regex

/**
 * Created by takeshita on 2014/06/04.
 */
object Implicits {

  implicit def toInjection( nameAndCode : (String,String)) = {
    Injection(nameAndCode._1,nameAndCode._2)
  }

}
