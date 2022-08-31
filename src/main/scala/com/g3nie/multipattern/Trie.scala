package com.g3nie.multipattern

import com.g3nie.util.Utils.Timer

/** {{{
 * Current version is implemented by prefix-Trie.
 *
 * todo:: Future enhancing
 *    Consider, Double-Array-Trie and/or Aho-Corasick.
 * }}}
 *
 */
case class Trie[T](label: String,
                   link: Map[Char, Trie[T]],
                   values: Seq[T] ) {
  def search(s: String): Seq[(T, Location)] = Trie.search(this)(s)
}
object Trie {

  import scala.collection.mutable.ListBuffer

  def apply[T](inputs: (String, T)*): Trie[T] = {

    def go(label: String = "^", values: List[T] = Nil)
             (inputs: (String, T)*)
    : Trie[T] = {

      val link =
        inputs
          .groupBy(_._1(0))
          .map { case (char, spt) =>
            val (vs, sub) =
              spt
                .map(st => (st._1.tail, st._2))
                .partition(_._1.isEmpty)
            val nl = label + char
            val nv = vs.map(t => t._2).toList

            char -> go(nl, nv)(sub: _*) // recursive make
          }

      new Trie(label, link, values)
    }

    go()(inputs: _*)
  }

  def search[T](trie: Trie[T])(target: String)
  : Seq[(T, Location)] = {

    // local mutation for performance..
    val to = target.length
    var (from, i, done, current) = (0, 0, false, trie)

    val buf = ListBuffer[(T, Location)]()

    while (from < to) {
      i = from
      done = false
      current = trie

      while (i < to && !done) {
        current.link.get(target.charAt(i)) match {
          case None =>
            done = true

          case Some(next) =>
            val l = Location(from, i+1, (i+1) < to)
            buf ++=  next.values.map ( t => t -> l)
            current = next
            i = i + 1
        }
      }
      from = from + 1
    }
    buf.toList
  }

}

object SpecTermTrie {

  def main(args: Array[String]): Unit = {

    val string =
      """startOfString
        |/** {{{
        |   * Current version is implemented by prefix-Trie.
        |   * Consider, Double-Array-Trie and/or Aho-Corasick.
        |   *
        |   * Term has location-condition ( start, end, exact, contain)
        |   * term syntax : "contain-term" , ^"start-term", "end-term"$, ^"exact-term"$ \"\"\"
        |   * }}}
        |   */
        |  case class TermTrie[T]( label: String,
        |                          link: Map[Char, TermTrie[T]],
        |                          values: List[(Located,T)] )
        |
        |  object TermTrie {
        |
        |    def apply[T](inputs: (String, T)*): TermTrie[T] = {
        |
        |      val positioned = inputs.filter( _._1.nonEmpty)
        |        .map( st => {
        |          val (s, p) = Located(st._1)
        |          (s, p, st._2)
        |        })
        |
        |      make()(positioned: _*)
        |    }
        |
        |    def search1[T]( trie: TermTrie[T])(target: String, from: Int)
        |    : Seq[(T, Location)] = {
        |
        |      // local mutation for performance..
        |      val to = target.length
        |      var (i, done, current) = (from, false, trie)
        |
        |      val buf = ListBuffer[(T, Location)]()
        |
        |      while( i < to && !done) {
        |
        |        current.link.get( target.charAt(i) ) match {
        |          case None =>
        |            done = true
        |
        |          case Some(next) =>
        |            val vs = take( next.values, i, from, to)
        |            buf.appendAll( vs )
        |            current = next
        |            i = i+1
        |        }
        |      }
        |      buf.toList
        |    }
        |
        |    def search[T]( trie: TermTrie[T])(target: String)
        |    : Seq[(T, Location)] = {
        |
        |      // local mutation for performance..
        |      val to = target.length
        |      var (from, i, done, current) = (0, 0, false, trie)
        |
        |      val buf = ListBuffer[(T, Location)]()
        |
        |      while( from < to ){
        |        i = from
        |        while( i < to && !done) {
        |
        |          current.link.get( target.charAt(i) ) match {
        |            case None =>
        |              done = true
        |
        |            case Some(next) =>
        |              val vs = take( next.values, i, from, to)
        |              buf.appendAll( vs )
        |              current = next
        |              i = i+1
        |          }
        |        }
        |        from = from +1
        |      }
        |      buf.toList
        |    }
        |
        |    private def take[T](pts: Seq[(Located, T)], i: Int, from: Int, to: Int)
        |    : Seq[(T, Location)] = {
        |
        |      if(pts.isEmpty)
        |        return Nil
        |
        |      val l = Location(from, i+1, (i+1) < to  )
        |      def loc(t: T) = Some( t -> l)
        |
        |      pts.flatMap {
        |        case (Wrong,_)   => None
        |        case (Contain,t) => loc(t)
        |        case (Start,t)   => if( l.start) loc(t) else None
        |        case (End,t)     => if( l.end)   loc(t) else None
        |        case (Exact,t)   => if( l.exact) loc(t) else None
        |      }
        |    }
        |
        |    private def make[T]( label: String = "^", values: List[(Located, T)] = Nil)
        |                       ( inputs: (String, Located, T)*)
        |    : TermTrie[T] = {
        |
        |      val link =
        |        inputs
        |          .groupBy( _._1(0))
        |          .map{ case (char, spt) =>
        |            val (vs, sub) =
        |              spt
        |                .map(st => (st._1.tail, st._2, st._3))
        |                .partition( _._1.isEmpty)
        |            val nl = label + char
        |            val nv = vs.map( t => (t._2, t._3) ).toList
        |
        |            char -> make( nl, nv)(sub: _*)        // recursive make
        |          }
        |
        |      new TermTrie(label, link, values)
        |    }
        |
        |    ///////////////////////////////////////////////////////////////////////////////
        |    sealed trait Located
        |    case object Start extends Located
        |    case object End extends Located
        |    case object Contain extends Located
        |    case object Exact extends Located
        |    case object Wrong extends Located  // Wrong-Position
        |
        |    object Located {
        |
        |      private val exact: Regex   = \"\"\"\s*\^"(.+)"\$\s*\"\"\".r
        |      private val start: Regex   = \"\"\"\s*\^"(.+)"\s*\"\"\".r
        |      private val end: Regex     = \"\"\"\s*"(.+)"\$\s*\"\"\".r
        |      private val contain: Regex = \"\"\"\s*"(.+)"\s*\"\"\".r
        |      private val usage = \"\"\"Syntax error: "contain-term" , ^"start-term", "end-term"$, ^"exact-term"$ \"\"\"
        |
        |      def apply(str: String, throwException: Boolean = false): (String, Located) = str match {
        |
        |        case exact(s)   => (s, Exact)     // ^"keyword"$
        |        case start(s)   => (s, Start)     // ^"keyword"$
        |        case end(s)     => (s, End)       // "keyword"$
        |        case contain(s) => (s, Contain)    // "keyword"
        |        case s          => if(throwException) throw new Exception( s"$usage\nyour string: $s ") else (s, Wrong)
        |      }
        |
        |      def test(): Unit = {
        |
        |        def go(st: String , expect: Located) = println( {
        |          val (s, p) = Located(st)
        |          s"expected Result: ${p ==expect}\tout: $s"
        |        })
        |
        |        go( \"\"\" ^"["exact"]"$  \"\"\", Exact)
        |        go( \"\"\" ^"["start"]"  \"\"\", Start)
        |        go( \"\"\" "["end"]"$  \"\"\", End)
        |        go( \"\"\" "["contain"]"  \"\"\", Contain)
        |        go( \"\"\" " "["contain-between double-quote"]" " \"\"\", Contain)
        |        go( \"\"\" err " string- before double-quote" " \"\"\", Wrong)
        |        go( \"\"\" " string- after double-quote" " err \"\"\", Wrong)
        |      }
        |    }
        |  }
        |endOfString""".stripMargin

    println(s"string.size : ${string.length}")
    println("================================================================================")

    val terms0 =
      string.split("\\s+")
        .filterNot(_.length < 3)
        .map(_.flatMap(c => if (c == '"') "\\\"" else c.toString)) // escaping "

    val terms1 =
      terms0
        .distinct
        .map(s => s -> s)

    val terms = terms1
    println(s"terms.size : ${terms.length}")
    println("================================================================================")

    val trie = Trie(terms: _*)
    val (result, _) = Timer { Trie.search(trie)(string) }

    println(s"found : ${result.size}")

    println("================================================================================")
    result.groupBy(_._1).foreach { case (_, v) =>
      println("")
      v foreach println
    }
  }

}

