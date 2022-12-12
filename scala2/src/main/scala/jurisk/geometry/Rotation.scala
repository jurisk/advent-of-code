package jurisk.geometry

sealed trait Rotation
object Rotation {
  case object Left90     extends Rotation
  case object NoRotation extends Rotation
  case object Right90    extends Rotation
  case object TurnAround extends Rotation
}
