package dk.itu.formhelper

trait Styles {
  sealed abstract class Style {
    def &&(style: Style) = AndStyle(this, style)
  }
  case class AndStyle(s1: Style, s2: Style) extends Style

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

  case object SameLine extends Style
  case object ShowRequirements extends Style
  case object ShowErrors extends Style
  case object Checked extends Style
}