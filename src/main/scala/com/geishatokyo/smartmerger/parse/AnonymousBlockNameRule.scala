package com.geishatokyo.smartmerger.parse

/**
 * Created by takeshita on 2014/06/04.
 */
trait AnonymousBlockNameRule {
  def prefix : String
  def getName(id : Long) = prefix + id
}

object AnonymousBlockNameRule extends AnonymousBlockNameRule{
  override def prefix: String = "$block"
}