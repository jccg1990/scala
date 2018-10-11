package week4

import week3.BankAccount

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  override def handler(pub: Publisher): Unit = compute()

  private def compute() =
    total = observed.map(_.currentBalance).sum

  def totalBalance = total
}
