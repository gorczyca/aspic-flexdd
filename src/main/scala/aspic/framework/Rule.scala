package aspic.framework

case class Rule(head: String, body: Set[String], isStrict: Boolean, label: Option[String]) {

  def this(head: String, body: Set[String]) = {
    this(head, body, true, None)
  }

  lazy val statements: Set[String] = body + head

  override def toString: String = s"${ label match
    case Some(label) => s"($label)"
    case _ => ""
  } $head ← ${body.mkString(",")}"

}

