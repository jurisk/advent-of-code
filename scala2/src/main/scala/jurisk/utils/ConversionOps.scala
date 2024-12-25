package jurisk.utils

import jurisk.utils.Parsing.StringOps

object ConversionOps {
  implicit class IntOps(value: Int) {
    def toBooleanStrict01Unsafe: Boolean = value match {
      case 0 => false
      case 1 => true
      case _ => s"Cannot convert $value to boolean".fail
    }
  }
}
