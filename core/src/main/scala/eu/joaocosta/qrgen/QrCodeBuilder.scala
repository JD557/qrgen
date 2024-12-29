package eu.joaocosta.qrgen;

import java.util.Arrays
import eu.joaocosta.qrgen.Helpers.*

final class QrCodeBuilder(val size: Int) {
  // The modules of this QR Code (false = light, true = dark).
  private val modules: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)
  // Indicates function modules that are not subjected to masking
  private var isFunction: Array[Array[Boolean]] = Array.ofDim[Boolean](size, size)

  def darkPoints: Int = modules.view.map(_.count(identity)).sum
  val totalPoints: Int = size * size

  def getModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x)
  }

  def setModule(x: Int, y: Int, isDark: Boolean): Unit = {
    modules(y)(x) = isDark
  }

  def updateModule(x: Int, y: Int, f: Boolean => Boolean): Unit =
    setModule(x, y, f(getModule(x, y)))

  def getFunctionModule(x: Int, y: Int): Boolean = {
    0 <= x && x < size && 0 <= y && y < size && isFunction(y)(x)
  }

  def setFunctionModule(x: Int, y: Int, isDark: Boolean): Unit = {
    modules(y)(x) = isDark
    isFunction(y)(x) = true
  }

  def result(): Array[Array[Boolean]] = modules.map(_.clone())
}
