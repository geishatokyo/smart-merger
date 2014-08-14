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
  def forJava = {
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

/**
 * マージの一連の処理を行うクラス
 * @param parser
 * @param merger
 */
case class Merger(parser : MarkerParser,merger : Injector){

  import com.geishatokyo.codegen.util.RichFile

  /**
   * 指定したファイルのreplaceブロック内を置き換える。
   * @param filePath
   * @param codeToReplace
   * @param codeIfFileNotFound
   * @return
   */
  def replaceMerge( filePath : String, codeToReplace : InjectionData,codeIfFileNotFound : String) : String = {
    replaceMerge(new File(filePath),codeToReplace,codeIfFileNotFound)
  }

  /**
   * 指定したファイルのreplaceブロック内を置き換える。
   * @param _file
   * @param codeToReplace
   * @param codeIfFileNotFound
   * @return
   */
  def replaceMerge( _file : File, codeToReplace : InjectionData,codeIfFileNotFound : String) : String = {
    val file = RichFile.fromFile(_file)
    var before = ""
    val parsedData = if(file.exists()){
      before = file.readAsString()
      parser.parse(before)
    }else{
      Logger.log("Create new file: " + file.getAbsolutePath)
      parser.parse(codeIfFileNotFound)
    }
    val s =merger.merge(parsedData,codeToReplace).rawString
    if(s != before) {
      Logger.log("Merge file: " + file.getAbsolutePath)
      file.write(s)
      s
    }else{
      Logger.log("File:" + file.getAbsolutePath + " is not changed")
      s
    }
  }
  def holdMerge( filePath : String, generatedCode : String) : String = {
    holdMerge(new File(filePath),generatedCode)
  }

  /**
   * 指定したファイルのholdブロック内を保持したまま、それ以外を置き換える
   * @param _file
   * @param generatedCode
   * @return
   */
  def holdMerge( _file : File, generatedCode : String) : String = {
    val file = RichFile.fromFile(_file)
    if(file.exists()){
      val before = file.readAsString()
      val base = parser.parse(before)
      val toMerge = parser.parse(generatedCode)
      val merged = merger.merge(base,toMerge)
      if(merged.rawString != before) {
        Logger.log("Merge file: " + file.getAbsolutePath)
        file.write(merged.rawString)
        merged.rawString
      }else{
        Logger.log("File:" + file.getAbsolutePath + " is not changed")
        before
      }
    }else{
      Logger.log("Create new file: " + file.getAbsolutePath)
      file.write(generatedCode)
      generatedCode
    }
  }

}


