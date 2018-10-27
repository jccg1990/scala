object UniqueId {
  val x = new AnyRef {}
  private var uidCount = 0L

  def startThread(): Thread = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId()
        println(uids)
      }
    }
    t.start()
    t
  }

  def getUniqueId(): Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

  val t = startThread()
  val s = startThread()
  t.join()
  s.join()

}
