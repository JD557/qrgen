package eu.joaocosta.qrgen.internal

object Helpers {
  def getBit(x: Int, i: Int): Boolean = ((x >>> i) & 1) != 0
}
