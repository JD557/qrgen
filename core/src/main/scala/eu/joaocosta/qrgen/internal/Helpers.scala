package eu.joaocosta.qrgen.internal

/** Object with internal helpers that do not fit anywhere else.
  */
object Helpers {

  /** Returns the value of bit i in number x
    */
  def getBit(x: Int, i: Int): Boolean = ((x >>> i) & 1) != 0
}
