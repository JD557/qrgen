package eu.joaocosta.qrgen

object Helpers {
  def getBit(x: Int, i: Int): Boolean = ((x >>> i) & 1) != 0
}
