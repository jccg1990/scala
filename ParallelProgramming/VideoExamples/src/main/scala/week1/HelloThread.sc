object HelloThread {
  class HelloThread extends Thread {
    override def run() {
      println("Hello")
      println("World!")
    }
  }

  val t = new HelloThread
  val s = new HelloThread
  t.start()
  s.start()
  t.join()
  s.join()
}