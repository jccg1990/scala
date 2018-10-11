import week4.BankAccountSignals
import week4.frp.Signal

object accounts {
  def consolidated(accts: List[BankAccountSignals]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  val a = new BankAccountSignals()
  val b = new BankAccountSignals()
  val c = consolidated(List(a,b))
  c()
  a deposit(20)
  c()
  b deposit(30)
  c()

  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())
  inDollar()
  b withdraw(10)
  inDollar()
}