package com.geishatokyo.smartmerger

import com.geishatokyo.smartmerger.injection.{Injection, InjectionRule, Injector}
import com.geishatokyo.smartmerger.parse.{MarkerParser, BlockParser}
import java.io.{FileNotFoundException, File}
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

  /**
   * <!--@insert[hoge]-->
   *
   * <!--@replace[name]-->
   * <!--@end-->
   *
   * <!--@hold[fuga]-->
   * <!--@end-->
   *
   * タイプのタグを使用
   * @return
   */
  def forXML = {
    val parser = MarkerParser.xmlParser()
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
   * 2つのファイルをマージする
   * @param baseFilePath
   * @param mergeFilePath
   * @return
   */
  def merge(baseFilePath : String,mergeFilePath : String) : String = {
    val base = RichFile.fromPath(baseFilePath)
    val merge = RichFile.fromPath(mergeFilePath)
    if(!base.exists()){
      merge.copyTo(base)
      return merge.readAsString()
    }
    if(!merge.exists){
      throw new Exception(s"Merge file:${base.getAbsolutePath} not found")
    }

    val baseData = parser.parse(base.readAsString())
    val mergeData = parser.parse(merge.readAsString())

    val merged = baseData.topLevel match{
      case TopLevel.Hold => {
        merger.inject(mergeData,baseData)
      }
      case TopLevel.Replace => {
        merger.inject(baseData,mergeData)
      }
      case TopLevel.None => {
        merge.copyTo(base)
        mergeData
      }
    }

    merged.rawString
  }

  /**
   * 指定したファイルのreplaceブロック内を置き換える。
   * @param filePath
   * @param codeToReplace
   * @return
   */
  def replaceMerge( filePath : String, codeToReplace :  List[Injection]) : String = {
    replaceMerge(new File(filePath),codeToReplace)
  }

  /**
   * 指定したファイルのreplaceブロック内を置き換える。
   * @param _file
   * @param codeToReplace
   * @return
   */
  def replaceMerge( _file : File, codeToReplace : List[Injection]) : String = {
    val file = RichFile.fromFile(_file)
    var before = ""
    val parsedData = if(file.exists()){
      before = file.readAsString()
      parser.parse(before)
    }else{
      throw new FileNotFoundException(s"Target file:${_file.getAbsoluteFile} not found")
    }
    if(parsedData.topLevel == TopLevel.Hold){
      throw new Exception(s"Top level of ${file.getName} is not replace")
    }
    val s =merger.inject(parsedData,codeToReplace).rawString
    if(s != before) {
      Logger.change("Merge file: " + file.getAbsolutePath)
      file.write(s)
      s
    }else{
      Logger.notChange("File:" + file.getAbsolutePath + " is not changed")
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

      if(base.topLevel == TopLevel.Replace){
        throw new Exception(s"Top level of ${file.getName} is not hold")
      }
      val merged = merger.inject(toMerge,base)

      if(merged.rawString != before) {
        Logger.change("Merge file: " + file.getAbsolutePath)
        file.write(merged.rawString)
        merged.rawString
      }else{
        Logger.notChange("File:" + file.getAbsolutePath + " is not changed")
        before
      }
    }else{
      Logger.change("Create new file: " + file.getAbsolutePath)
      file.write(generatedCode)
      generatedCode
    }
  }

}


