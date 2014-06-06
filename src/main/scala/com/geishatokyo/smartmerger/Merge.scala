package com.geishatokyo.smartmerger

import com.geishatokyo.smartmerger.merge.{MergeData, MergeRule, Merger}
import com.geishatokyo.smartmerger.parse.{MarkerParser, BlockParser}
import java.io.File
import com.geishatokyo.codegen.util.{Logger, FileUtil}

/**
 * Created by takeshita on 2014/06/05.
 */
object Merge {

  var mergeRule = new MergeRule

  def forScala = {
    val parser = MarkerParser.doubleSlashParser()
    val merger = new Merger(mergeRule)
    Merge(parser,merger)
  }

  def forCSharp = {
    val parser = MarkerParser.doubleSlashParser()
    val merger = new Merger(mergeRule)
    Merge(parser,merger)
  }

  def forMySQL = {
    val parser = MarkerParser.doubleSharpParser()
    val merger = new Merger(mergeRule)
    Merge(parser,merger)
  }

}

case class Merge(parser : MarkerParser,merger : Merger){

  import com.geishatokyo.codegen.util.RichFile
  def replaceMerge( filePath : String, codeToReplace : MergeData,codeIfFileNotFound : String) : String = {
    val file = RichFile.fromString(filePath)
    val parsedData = if(file.exists()){
      Logger.log("Merge file: " + filePath)
      parser.parse(file.readAsString())
    }else{
      Logger.log("Create new file: " + filePath)
      parser.parse(codeIfFileNotFound)
    }
    val s =merger.merge(parsedData,codeToReplace).rawString
    file.write(s)
    s
  }
  def holdMerge( filePath : String, generatedCode : String) : String = {
    val file = RichFile.fromString(filePath)
    if(file.exists()){
      val base = parser.parse(file.readAsString())
      val toMerge = parser.parse(generatedCode)
      val merged = merger.merge(base,toMerge)
      Logger.log("Merge file: " + filePath)
      file.write(merged.rawString)
      merged.rawString
    }else{
      Logger.log("Create new file: " + filePath)
      file.write(generatedCode)
      generatedCode
    }
  }

}


