package com.g3nie.multipattern

import com.g3nie.multipattern.RuleAST.{Rule, RuleRoot, Term}
import com.g3nie.util.Utils.Timer

import scala.collection.immutable

/** {{{
 * << Multi-Pattern-Matching >>
 *
 * impl. note.
 *  ~ term matching :: Trie(current version)
 *                   later,  Double-Array-Trie, KMP ( Aho-Corasick )
 *  ~ provide DSL   :: Rule(AST and Parser )
 *
 *
 * }}}
 */
case class Occurrence( keyword: String, loc: Seq[Int]) {
  override def toString: String =
    loc.mkString( s"$keyword: ", ",", "" )
}

case class MultiPattern(rules: Map[Int, RuleRoot], dictionary: Trie[Term]) {

  def search(s: String): Iterable[(Int, String, Seq[Occurrence])] =  {

    def matched(ms: Seq[(Term, Location)]) = {
      ms.groupBy(_._1.keyword).map{ case (k, v) =>
        Occurrence( k, v.map( _._2.from))
      }.toSeq
    }

    val ret =
      dictionary
        .search(s)
        .groupBy( _._1.root)
        .flatMap { case (root, ms: Seq[(Term, Location)]) =>
          rules.get(root).flatMap { r =>
            val p = (i: Int) => ms.exists { case (t, l) => (t.id == i) }  // todo :: location check
            val f = Rule.foldWith(r.rule)(p, () => matched(ms))
            f.map( (root, r.string, _))
          }
        }
    ret
  }
}

object MultiPattern {

  def apply( ss: (Int, String)*): MultiPattern = {

    val rs = ss.flatMap(s => RuleRoot( s._1, s._2))

    val rules = rs.groupBy( _.id).view.mapValues( s =>
      if(s.isDefinedAt(1)) throw new Exception( s.mkString( "error : duplicated ruleId ", "," ,"" )  )
      else s.head
    ).toMap

    val terms = rs.flatMap(_.terms ).map( t => t.keyword -> t)

    val dictionary = Trie( terms:_*)

    new MultiPattern( rules, dictionary)
  }
}

object SpecMultiPattern extends App {

  val src =
    """
      |case class Occurrence( keyword: String, loc: Seq[Int]) {
      |  override def toString: String =
      |    loc.mkString( s"$keyword: ", ",", "" )
      |}
      |
      |case class MultiPattern(rules: Map[Int, RuleRoot], dictionary: Trie[Term]) {
      |
      |  def search(s: String) =  {
      |
      |    def matched(ms: Seq[(Term, Location)]) = {
      |      ms.groupBy(_._1.keyword).map{ case (k, v) =>
      |        Occurrence( k, v.map( _._2.from))
      |      }.toSeq
      |    }
      |
      |    val ret =
      |      dictionary
      |        .search(s)
      |        .groupBy( _._1.root)
      |        .flatMap { case (root, ms: Seq[(Term, Location)]) =>
      |          rules.get(root).flatMap { r =>
      |            val p = (i: Int) => ms.exists { case (t, l) => (t.id == i) }  // todo :: location check
      |            val f = Rule.foldWith(r.rule)(p, () => matched(ms))
      |            f.map( (root, r.string, _))
      |          }
      |        }
      |    ret
      |  }
      |}
      |
      |object MultiPattern {
      |
      |  def apply( ss: (Int, String)*) = {
      |
      |    val rs = ss.flatMap(s => RuleRoot( s._1, s._2))
      |
      |    val rules = rs.groupBy( _.id).view.mapValues( s =>
      |      if(s.isDefinedAt(1)) throw new Exception( s.mkString( "error : duplicated ruleId ", "," ,"" )  )
      |      else s.head
      |    ).toMap
      |
      |    val terms = rs.flatMap(_.terms ).map( t => t.keyword -> t)
      |
      |    val dictionary = Trie( terms:_*)
      |
      |    new MultiPattern( rules, dictionary)
      |  }
      |}
      |""".stripMargin

  val ps0 =
    src.split("\\s+")
      .map( s => s.filterNot( c => "\\{}():=>[]!~\",|/'`&".contains(c)) )
      .filter( _.length > 2)
      .distinct
      .map( s => s"'$s'").toSeq

  ps0 foreach println
  println( ps0.size)

  val ps1 = Seq(
    "'val' ~ 'dictionary' ~ 'good'",
    "'TermsRuleRoot' & !'duplicated'",
  )
  val pat = ps0.zipWithIndex.map( i => i._2 -> i._1 )

  val mp = MultiPattern(pat:_*)

  println()
  println( s"src.size = ${src.length}")
  println( s"rule.size = ${pat.length}")
  println()

  Timer {
    mp.search(src).foreach(println)
  }
}

