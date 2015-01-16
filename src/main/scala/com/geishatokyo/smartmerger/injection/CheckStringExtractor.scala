package com.geishatokyo.smartmerger.injection

/**
 * Created by takeshita on 2015/01/16.
 */
trait CheckStringExtractor {

  def getCheckString( code : String) : Option[String]
  def not = true
}
