package week3

abstract class Simulation {
  type Action = () => Unit
  private var curtime = 0
  private var agenda: List[Event] = List()

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run() {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: ag
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  case class Event(time: Int, action: Action)

}
