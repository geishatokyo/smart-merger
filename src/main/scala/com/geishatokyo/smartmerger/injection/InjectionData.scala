package com.geishatokyo.smartmerger.injection

import com.geishatokyo.smartmerger.TopLevel
import com.geishatokyo.smartmerger.parse.AnonymousBlockNameRule

/**
 * Created by takeshita on 2014/06/03.
 */
case class InjectionData(blocks : InjectionBlock*) {

  val nameRule = AnonymousBlockNameRule

  private lazy val nameToBlocks = {

    var id = 0
    blocks.map(b => b.name match{
      case Some(name) => name -> b
      case None => {
        id += 1
        nameRule.getName(id) -> b
      }
    }).groupBy(_._1).mapValues(_.map(_._2))

  }

  lazy val blockNames = nameToBlocks.keys.toSet

  def getBlocks(name : String) = {
    nameToBlocks.getOrElse(name,Nil)
  }

}
