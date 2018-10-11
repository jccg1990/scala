package week4

trait Subscriber {
  def handler(pub: Publisher)
}
