package jurisk.geometry

sealed trait Rotation {
  def inverse: Rotation
}

object Rotation {
  case object Left90     extends Rotation {
    override def inverse: Rotation = Right90
  }
  case object NoRotation extends Rotation {
    override def inverse: Rotation = NoRotation
  }
  case object Right90    extends Rotation {
    override def inverse: Rotation = Left90
  }
  case object TurnAround extends Rotation {
    override def inverse: Rotation = TurnAround
  }
}
