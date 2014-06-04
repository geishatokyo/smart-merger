package com.geishatokyo.codegen.util

/**
 * Created with IntelliJ IDEA.
 * User: takezoux2
 * Date: 2013/11/28
 * Time: 15:58
 * To change this template use File | Settings | File Templates.
 */
object StringUtil {


  def camelToSnake(str : String) = separateCamelCase(str,"_")
  def camelToHyphen(str : String) = separateCamelCase(str,"-")
  def camelToDot(str : String) = separateCamelCase(str,".")

  def separateCamelCase( str : String, separator : String) : String = {
    if (str == null) return null
    if (str.length == 0) return ""
    val b = new StringBuilder(str.length + 5 * separator.length)

    val chars = str.toCharArray
    b.append(chars(0))

    for (i <- 1 until str.length){
      val c =  chars(i)
      if (Character.isUpperCase(c)){

        val beforeC = chars(i-1)

        try{
          val nextC = chars(i + 1)
          if (Character.isUpperCase(beforeC)){
            if (Character.isUpperCase(nextC)){
              b.append(c)
            }else{
              b.append(separator)
              b.append(Character.toLowerCase(c))
            }
          }else{
            if (Character.isUpperCase(nextC)){
              b.append(separator)
              b.append(c)
            }else{
              b.append(separator)
              b.append(Character.toLowerCase(c))
            }
          }
        }catch{
          case e : ArrayIndexOutOfBoundsException => {
            // end of string
            if (Character.isUpperCase(beforeC)){
              b.append(c)
            }else{
              b.append(separator)
              b.append(Character.toLowerCase(c))
            }
          }
        }
      }else{
        b.append(c)
      }
    }

    b.toString()
  }

  def toCamelCase(str : String, separator : String) : String = {
    if (str == null) return null
    val v = str.split(separator)
    if (v.length > 0){
      v(0) + v.drop(1).map(s => {
        s.capitalize
      }).mkString

    }else{
      ""
    }
  }

  def decapitalize(str : String) : String = {
    if (str == null) return null
    if (str.length == 0) return str
    val c = str.charAt(0)
    if (Character.isUpperCase(c)){
      Character.toLowerCase(c) + str.substring(1)
    }else{
      str
    }
  }

}
