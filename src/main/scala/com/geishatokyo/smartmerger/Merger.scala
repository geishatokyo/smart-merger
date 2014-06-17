package com.geishatokyo.smartmerger

import com.geishatokyo.smartmerger.injection.{InjectionData, InjectionRule, Injector}
import com.geishatokyo.smartmerger.parse.{MarkerParser, BlockParser}
import java.io.File
import com.geishatokyo.codegen.util.{Logger, FileUtil}

/**
 * Created by takeshita on 2014/06/05.
 */
object Merger {

  var mergeRule = new InjectionRule

  /**
   * //@insert[hoge]
   *
   * //@replace[name]
   * //@end
   *
   * //@hold[fuga]
   * //@end
   *
   * タイプのタグを使用
   * @return
   */
  def forScala = {
    val parser = MarkerParser.doubleSlashParser()
    val merger = new Injector(mergeRule)
    Merger(parser,merger)
  }
  /**
   * //@insert[hoge]
   *
   * //@replace[name]
   * //@end
   *
   * //@hold[fuga]
   * //@end
   *
   * タイプのタグを使用
   * @return
   */
  def forCSharp = {
    val parser = MarkerParser.doubleSlashParser()
    val merger = new Injector(mergeRule)
    Merger(parser,merger)
  }
  /**
   * ##insert[hoge]
   *
   * ##replace[name]
   * ##end
   *
   * ##hold[fuga]
   * ##end
   *
   * タイプのタグを使用
   * @return
   */
  def forMySQL = {
    val parser = MarkerParser.doubleSharpParser()
    val merger = new Injector(mergeRule)
    Merger(parser,merger)
  }

  /**
   * ##insert[hoge]
   *
   * ##replace[name]
   * ##end
   *
   * ##hold[fuga]
   * ##end
   *
   * タイプのタグを使用
   * @return
   */
  def forRuby = {
    val parser = MarkerParser.doubleSharpParser()
    val merger = new Injector(mergeRule)
    Merger(parser,merger)
  }

}

case class Merger(parser : MarkerParser,merger : Injector){

  import com.geishatokyo.codegen.util.RichFile
  def replaceMerge( filePath : String, codeToReplace : InjectionData,codeIfFileNotFound : String) : String = {
    replaceMerge(new File(filePath),codeToReplace,codeIfFileNotFound)
  }
  def replaceMerge( file : File, codeToReplace : InjectionData,codeIfFileNotFound : String) : String = {
    val file = RichFile.fromFile(file)
    val parsedData = if(file.exists()){
      Logger.log("Merge file: " + file.getAbsolutePath)
      parser.parse(file.readAsString())
    }else{
      Logger.log("Create new file: " + file.getAbsolutePath)
      parser.parse(codeIfFileNotFound)
    }
    val s =merger.merge(parsedData,codeToReplace).rawString
    file.write(s)
    s
  }
  def holdMerge( filePath : String, generatedCode : String) : String = {
    holdMerge(new File(filePath),generatedCode)
  }
  def holdMerge( file : File, generatedCode : String) : String = {
    val file = RichFile.fromFile(file)
    if(file.exists()){
      val base = parser.parse(file.readAsString())
      val toMerge = parser.parse(generatedCode)
      val merged = merger.merge(base,toMerge)
      Logger.log("Merge file: " + file.getAbsolutePath)
      file.write(merged.rawString)
      merged.rawString
    }else{
      Logger.log("Create new file: " + file.getAbsolutePath)
      file.write(generatedCode)
      generatedCode
    }
  }

}


