package dk.itu.formhelper

trait Styles {
  sealed abstract class Style extends Ordered[Style]{
    def &&(style: Style) = AndStyle(this, style)
    def compValue: Int
    def compare(that: Style) = that.compValue - compValue
  }
  case class AndStyle(s1: Style, s2: Style) extends Style {
    val compValue = 0
  }

  sealed trait Placement
  case object After extends Placement
  case object Before extends Placement
  case object Inside extends Placement

  final case class Label(label: String, placement: Placement) extends Style {
    val compValue = 1
  }
  object Label extends Style {
    val compValue = 1
    def <(label: String) = Label(label, Before)
    def >(label: String) = Label(label, After)
    def <>(label: String) = Label(label, Inside)
  }

  case object SameLine extends Style {
    val compValue = 2
  }
  case object Checked extends Style {
	  val compValue = 3
  }
  case object ShowRequirements extends Style {
    val compValue = 4
  }
  case object ShowErrors extends Style {
    val compValue = 5
  }
  
  final case class Error(err: String) extends Style {
    val compValue = 6
  }
  object Error extends Style {
    val compValue = 6
    def ===(err: String) = Error(err)
  }
}