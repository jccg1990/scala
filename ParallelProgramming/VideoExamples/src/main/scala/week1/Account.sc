object Account {
  val x = new AnyRef {}
  private var uidCount = 0L

  def getUniqueId: Long = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

  class Account(private var amount: Int = 0){
    val uid = getUniqueId

    def lockAndTransfer(target: Account, n: Int): Unit =
      this.synchronized{
        target.synchronized{
          this.amount -= n
          target.amount += n
        }
      }

    def transfer(target: Account, n: Int): Unit ={
      if (this.uid < target.uid) this.lockAndTransfer(target, n)
      else target.lockAndTransfer(this, -n)
    }
  }

  def startThread(a: Account, b: Account, n: Int) = {
    val t = new Thread {
      override def run(): Unit = {
        for (i <- 0 until n ) {
          a.transfer(b,1)
        }
      }
    }
    t.start()
    t
  }

  val a1 = new Account(50000)
  val a2 = new Account(70000)

  val t = startThread(a1, a2, 150000)
  val s = startThread(a2, a1, 150000)

  t.join()
  s.join()

  println("finished")
}