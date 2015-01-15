package com.geishatokyo.smartmerger.injection

import com.geishatokyo.smartmerger.parse.{BlockWithStartTag, ParsedData}

/**
 * Created by takeshita on 2015/01/15.
 */
object InjectionConverter {

  def toInjections(parsedData : ParsedData) : Seq[Injection] = {
    parsedData.blocks.collect({
      case b : BlockWithStartTag => {
        Injection.Always(b.name,b.text)
      }
    })
  }

}
