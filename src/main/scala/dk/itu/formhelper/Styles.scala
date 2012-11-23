package dk.itu.formhelper

trait Styles {
  trait Style

  sealed trait Placement
  case object After extends Placement
  case object Before extends Placement
  case object Inside extends Placement

  final case class Label(label: String, placement: Placement) extends Style
  object Label extends Style {
    def <(label: String) = Label(label, Before)
    def >(label: String) = Label(label, After)
    def <>(label: String) = Label(label, Inside)
  }

  object SameLine extends Style
  
  case object ShowRequirements extends Style
  case object ShowErrors extends Style

  final case class Error(msg: String) extends Style
  case object Error extends Style {
    def ==(msg: String) = Error(msg)
  }
}