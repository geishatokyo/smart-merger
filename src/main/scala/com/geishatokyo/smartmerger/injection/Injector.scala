package com.geishatokyo.smartmerger.injection

import com.geishatokyo.smartmerger.parse._
import com.geishatokyo.smartmerger.{Env, TopLevel}

/**
 * Created by takeshita on 2014/06/03.
 */
object Injector{

  def apply() : Injector = Injector(new InjectionRule())
}


case class Injector(mergeRule : InjectionRule) {

  def inject(baseFile : ParsedData,injectionData : ParsedData) : ParsedData = {
    if(baseFile.blocks.exists(_.isInstanceOf[SkipMerge])){
      return baseFile
    }

    if(baseFile.topLevel != injectionData.topLevel){
      throw new Exception(s"Base file top level is ${baseFile.topLevel}, but injection file top level is not.")
    }

    inject(baseFile,InjectionConverter.toInjections(injectionData))

  }

  def inject(baseFile : ParsedData,injections : Seq[Injection]) : ParsedData = {

    val grouped = injections.groupBy(_.name)

    val blocks = baseFile.blocks.map({
      case t : BlockWithStartTag => {
        grouped.get(t.name) match{
          case Some(injections) => {
            val v = injections.filter(_.willMerge(baseFile))
            val text = v.map(i => {
              if(i.autoIndent){
                i.text.lines.map(l => (" " * t.indent) + l).mkString(Env.lineSeparator)
              }else {
                i.text
              }
            }).mkString(Env.lineSeparator)

            t.copyWithText(text)
          }
          case None => {
            t
          }
        }
      }
      case b => b
    })

    ParsedData(blocks :_*)
  }
}
