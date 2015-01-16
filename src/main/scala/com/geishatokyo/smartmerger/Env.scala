package com.geishatokyo.smartmerger

/**
 * Created by takeshita on 2015/01/16.
 */
object Env {

  var lineSeparator = System.lineSeparator()

  /**
   * デバッグログを出力する
   * default : true
   */
  var printDebugLog = true
  /**
   * 変更に関するログを出力する
   * default : true
   */
  var printChangeLog = true
  /**
   * 変更が無い場合のログを出力する
   * default : true
   */
  var printNotChangeLog = true

}
