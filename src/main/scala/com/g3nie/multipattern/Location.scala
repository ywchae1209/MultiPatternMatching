package com.g3nie.multipattern

/**
 *
 * {{{
 * Matched Location implies matching position information.
 * such as,
 *      is startWith ?  from == 0
 *      is endWith  ? !remain
 *      is exactMath ?  start && end
 *      is contain ? always true
 *
 * }}}
 */
case class Location( from: Int, to: Int, remain: Boolean)
