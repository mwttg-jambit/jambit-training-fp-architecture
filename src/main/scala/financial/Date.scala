package financial

import java.time.LocalDateTime

final case class Date(desc: String) {
  def isAfter(now: Date): Boolean = {
    val x = LocalDateTime.parse(this.desc)
    val y = LocalDateTime.parse(now.desc)

    y.isAfter(x)
  }
}
