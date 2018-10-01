object test {

  val f: PartialFunction[String, String] = {
    case "ping" => "pong"
  }

  f("ping")
  f.isDefinedAt("ping")
}