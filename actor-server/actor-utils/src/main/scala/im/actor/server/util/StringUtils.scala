package im.actor.server.util

import java.nio.charset.Charset
import java.util.regex.Pattern

import cats.data.Xor

object StringUtils {

  private val encoder = Charset.forName("US-ASCII").newEncoder()

  def utfToHexString(s: String): String = { s.map(ch ⇒ f"${ch.toInt}%04X").mkString }

  def isAsciiString(c: CharSequence): Boolean = encoder.canEncode(c)

  def nonEmptyString(s: String): String Xor String = {
    val trimmed = s.trim
    if (trimmed.isEmpty) Xor.left("Should be nonempty") else Xor.right(trimmed)
  }

  def printableString(s: String): String Xor String = {
    val p = Pattern.compile("\\p{Print}+", Pattern.UNICODE_CHARACTER_CLASS)
    if (p.matcher(s).matches) Xor.right(s) else Xor.left("Should contain printable characters only")
  }

  def validName(n: String): String Xor String =
    nonEmptyString(n) flatMap printableString

}
