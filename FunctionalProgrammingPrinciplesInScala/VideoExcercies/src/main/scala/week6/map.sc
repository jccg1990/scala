object map {
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry get "US"
  capitalOfCountry get "Andorra"

  val fruit = List("apple", "pear", "orange", "pineapple")
  fruit.sortWith(_.length < _.length)
  fruit.sorted

  fruit.groupBy(_.head)
}