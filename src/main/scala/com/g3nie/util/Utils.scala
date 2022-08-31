package com.g3nie.util


object Utils {

  class Lazy[T]( calc0: () => T){
    private lazy val force = calc0()
    def apply(): T = force
  }

  def Timer[A](f: => A): (A, Long) = {
    val s = System.currentTimeMillis
    val r = f
    val e = System.currentTimeMillis
    println("spent(millis): " + (e - s))
    r -> (e - s)
  }

  def unescapeUnicode(str: String): String =

    """\\u+([0-9a-fA-F]{4})""".r.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar match {
        case '\\' => """\\"""
        case '$' => """\$"""
        case c => c.toString
      })

  def unescapeString( s: String): String = {

    StringContext.processEscapes( unescapeUnicode(s))
  }

}