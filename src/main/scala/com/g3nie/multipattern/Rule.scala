package com.g3nie.multipattern

/**
 * {{{
 * Field Analysis syntax
 *
 * 1) term = 'string' | ^'string' | ^'string'$ | 'string'$
 *          1. 'string'   : Target contains string.
 *          2. ^'string'  : Target starts with string.
 *          3. ^'string'$ : Target is exactly string
 *          4. 'string'$  : Target ends with string.
 *
 *        note 1) quote(') is used to be useful in Json's body.
 *                        ( double-quote(") need more escaping. )
 *        note 2) quote(') in string must be escaped. ( \' )
 *
 * 2) termSeq = ( term1 ~ term2 ~ term3 ~ ... )
 *          Target contains all term1,2,3... in sequential order.
 *
 * 3) parentheses = ( expr )
 *          used for evaluation precedence ordering.
 *
 * 4) braces = { expr }
 *          used for evaluation precedence ordering.
 *
 * 5) negate = ! expr
 *          negate expr
 *
 *          * see) not() function note
 *
 * 6) and = expr1 & expr2 & expr3...
 *          And operation between expr1,2,...
 *
 * 7) or = expr1 & expr2 & expr3...
 *          Or operation between expr1,2,...
 *
 * 8) ! = negate expression
 *         TermSeq need to be braced to be negated
 *            ! ( 'a' ~ 'b') :: ok
 *            ! 'a' ~ 'b'    :: syntax error
 *
 * note: mixed And/Or ? = And operator has higher precedence.
 *            A | B & C | D & E
 *        ==  A |(B & C)|(D & E)
 *
 * todo: Redundant And/Or ? automatically flatten
 *          A & ( B & ( C | D ) => A & B & ( C | D )
 *          A | ( B | ( C & D ) => A | B | ( C & D )
 * }}}
 */
object RuleAST {

  case class RuleRoot(id: Int, rule: Rule, terms: Seq[Term], string: String)

  object RuleRoot {

    def apply( root: Int, ruleString: String): Option[RuleRoot] = {

      Rule( root, ruleString).map( rule =>
        new RuleRoot(root, rule, rule.getAllTerms, rule.toString)
      )
    }

  }

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait TermType
  case object Exact extends TermType
  case object StartWith extends TermType
  case object EndWith extends TermType
  case object Contain extends TermType
  case object Lookup extends TermType

  ////////////////////////////////////////////////////////////////////////////////
  sealed trait TermsOpType
  case object Or extends TermsOpType
  case object And extends TermsOpType
  case object TermSeq extends TermsOpType

  ////////////////////////////////////////////////////////////////////////////////

  sealed trait Rule {
    val op: Boolean
    override def toString: String = Rule.toString(this)
    def getAllTerms: Seq[Term] = Rule.getAllTerms(this)
  }

  case class Term ( keyword: String, kind: TermType, op: Boolean = true, id: Int = 0, root: Int = 0) extends Rule
  case class TermsOp(children: Seq[Rule], kind: TermsOpType, op: Boolean = true) extends Rule

  ////////////////////////////////////////////////////////////////////////////////
  object TermsOp {

    def apply(c0: Rule, cs: Seq[Rule], kind: TermsOpType): Rule=
      if (cs.isEmpty) c0 else TermsOp( c0 +: cs, kind)
  }

  object Rule {

    def apply( root: Int, ruleString: String): Option[Rule] = {

      RuleParser( ruleString)
        .map( rule =>
          // term's Id and root-id is set after parsing
          setTermId( flatten(rule), root)
      )
    }

    // todo :: logic completion
    def foldWith[T](root: Rule)(f: Int => Boolean, all: () => T): Option[T] = {

      def go(tree: Rule): Boolean = tree match {
        case TermsOp( child, kind, op) =>  kind match {
          case Or       => if (op) child.exists(go) else !child.forall(go)
          case And      => if (op) child.forall(go) else !child.exists(go)
          case TermSeq  => if (op) child.forall(go) else !child.forall(go)
        }

        case Term( _, _, op, id, _) => if (op) f(id) else !f(id)
      }

      val ok = go(root)
      if (ok) Some(all()) else None
    }

    ////////////////////////////////////////////////////////////////////////////////
    /**
     * todo:: flatten Redundant Operation
     */
    def flatten(from: Rule) : Rule = from

    ////////////////////////////////////////////////////////////////////////////////
    private def getAllTerms(sr: Rule): Seq[Term] = {
      val buf = collection.mutable.ListBuffer[Term]()

      def go(t: Rule): Unit = t match {
        case TermsOp( child, _, _) => child.foreach(go)
        case l @Term( _, _, _, _, _) => buf += l
      }

      go(sr)
      buf.toList
    }

    private def setTermId(sr: Rule, root: Int): Rule = {

      var i = 0
      def go( n: Rule): Rule = {
        n match {
          case TermsOp( child, k, o) => TermsOp( child.map(go), k, o)
          case Term(s, k, o, _, _)     =>
            val ret = Term( s, k, o, i, root)
            i = i+1
            ret
        }
      }
      go(sr)
    }

    private def toString( t: Rule): String = t match {
      case TermsOp( child, kind, op) =>
        kind match {
          case Or => child.mkString(if (op) "(" else "!(", " | ", ")")
          case And => child.mkString(if (op) "(" else "!(", " & ", ")")
          case TermSeq => child.mkString(if (op) "(" else "!(", " ~ ", ")")
        }

      case Term( g, r, op, id, root) =>
        val o = if (op) "" else "!"
        val pp =
          r match {
            case Exact     => s"^$g$$"
            case StartWith => s"^$g"
            case EndWith   => s"$g$$"
            case Contain   => s"$g"
            case Lookup   => s"$$$$$${$g}$$$$$$"
          }
        //        s"$id:$o$pp"
        s"$o$pp"
    }
  }
}

import com.g3nie.multipattern.RuleAST._
object RuleParser  {

  /**
   *  base library : https://com-lihaoyi.github.io/fastparse/#GettingStarted
   */
  import fastparse._
  import NoWhitespace._
  import fastparse.{CharPred, CharsWhileIn, P}

  def apply( ruleString: String): Option[Rule] = {

    parse( ruleString, expression(_)) match {

      case Parsed.Success(rule, _) => Some( rule)
      case Parsed.Failure(_, index,  extra) =>

        // todo :: logging-level
        println(s"failed: at $index\n${extra.trace().msg}")

        None
    }
  }

  /**
   * {{{
   * note)
   *  when called from negate, this impl. is right. (current version.)
   *  but, when flatten( or mutate state), TermsOps update recursively (And --> OR, OR --> AND)
   * }}}
   */
  private def not( r: Rule): Rule = r match {
    case TermsOp( child, kind, op) => TermsOp( child, kind, !op)
    case Term( g, r, op, id, root) => Term( g, r, !op, id, root)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Rule-ADT
  ////////////////////////////////////////////////////////////////////////////////

  private def sp[_: P] = P( CharsWhileIn(" \r\n\t").rep(max = 5))
  private def `{`[_: P] = P("{" ~ sp)
  private def `}`[_: P] = P(sp ~ "}")
  private def `(`[_: P] = P("(" ~ sp)
  private def `)`[_: P] = P(sp ~ ")")
  private def `|`[_: P] = P(sp ~ "|" ~ sp)
  private def `&`[_: P] = P(sp ~ "&" ~ sp)
  private def `~`[_: P] = P(sp ~ "~" ~ sp)

  private def ch[_: P] = P(CharPred(_ != '\''))
  private def eq[_: P] = P("\\'".!)

  private def cl[_: P] = P(CharPred(_ != '}'))
  private def el[_: P] = P("\\}".!)

  ////////////////////////////////////////////////////////////////////////////////

  /**
   *  todo ::: Future spec
   *  note) Someday... lookup will be needed..
   *  i think, lookup is not term, but single type of expression.
   *  ( may not used as AST-Branch or Leaf, but as Root)
   *
   */
  private def lookup[_: P] = P("'${" ~ (el | cl).rep(1).! ~ "}'").map(s => Term( s, Lookup))

  private def exact[_: P]  = P("^'"  ~ (eq | ch).rep(1).! ~ "'$").map(s => Term( s, Exact))
  private def start[_: P]  = P("^'"  ~ (eq | ch).rep(1).! ~ "'" ).map(s => Term( s, StartWith))
  private def end[_: P]    = P("'"   ~ (eq | ch).rep(1).! ~ "'$").map(s => Term( s, EndWith))
  private def contain[_: P]= P("'"   ~ (eq | ch).rep(1).! ~ "'").map(s => Term( s, Contain))

  private def term[_: P]   = P( exact | start  | end | contain )
  private def termSeq[_: P]= P((term ~ `~`).rep(1) ~ term ).map(s => TermsOp( s._1 :+ s._2, TermSeq))

  private def parentheses[_: P] = P(`(` ~ or ~ `)`)
  private def braces[_: P]      = P(`{` ~ or ~ `}`)
  private def negate[_: P]      = P("!" ~ sp ~ (parentheses | braces | term)).map(s => not(s))

  private def token[_: P]: P[Rule] = P( termSeq | parentheses | braces | negate | term)
  private def and[_: P]: P[Rule] = P(token ~ (`&` ~/ token).rep).map(s => TermsOp(s._1, s._2, And))
  private def or[_: P]: P[Rule] = P(and ~ (`|` ~/ and).rep).map(s => TermsOp(s._1, s._2, Or))

  private def expression[_: P]: P[Rule] = P(sp ~ or ~ sp ~ End)

}

////////////////////////////////////////////////////////////////////////////////
object SpecFieldRuleAST extends App {

  val s =
    """ !('this' ~ 'is' ~ 'suspicious' ~ 'case') | !(!(!'test' & !{!('testing rule')})) |
      | !^'exact'$ | !'contain' | !'end'$ | !^'start' |
      | { !('a' & 'b') & ('c' | 'd') } |
      | ! { !('a' & 'b') | ('c' | 'd') } & ( 'e' | 'f' ) |
      | ('a' & ( 'b' & 'c' & ('d' & 'e'))) |
      | (((( 'a' & 'b' ) & 'c') & 'd' ) & 'e')
      |""".stripMargin

  val s1 = "!('a' & !( 'b' | !'c')) | ((!(  !( 'a' & 'b' ) & 'c') | 'd' ) & 'e') "
  val r = Rule(1000, s)

}
