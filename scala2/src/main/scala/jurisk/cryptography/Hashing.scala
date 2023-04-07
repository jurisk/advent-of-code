package jurisk.cryptography

import java.security.MessageDigest

object Hashing {
  def md5(text: String): String = {
    val digest = MessageDigest.getInstance("MD5")
    digest.digest(text.getBytes).map("%02x".format(_)).mkString
  }
}
