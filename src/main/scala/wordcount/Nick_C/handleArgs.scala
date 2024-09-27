class HandleArgs {
  def handleArgs(args: Array[String]): List[String] = {
    // Default values
    val defaults = Map(
      "cloud-size" -> 10,
      "length-at-least" -> 6,
      "window-size" -> 1000
    )

    // Collect arguments from the passed array
    val collectedArgs = args.grouped(2).collect {
      case Array("-c", value) => "cloud-size" -> value
      case Array("--cloud-size", value) => "cloud-size" -> value
      case Array("-l", value) => "length-at-least" -> value
      case Array("--length-at-least", value) => "length-at-least" -> value
      case Array("-w", value) => "window-size" -> value
      case Array("--window-size", value) => "window-size" -> value
    }.toList

    // Create a map with defaults and any provided arguments
    val argMap = collectedArgs.toMap.withDefault(identity)

    // Create the final argument map combining defaults and provided values
    val finalArgs = Map(
      "cloud-size" -> argMap.getOrElse("cloud-size", defaults("cloud-size").toString),
      "length-at-least" -> argMap.getOrElse("length-at-least", defaults("length-at-least").toString),
      "window-size" -> argMap.getOrElse("window-size", defaults("window-size").toString)
    )

    // Create result list to return
    finalArgs.toList.map {
      case (key, value) => s"$key: $value"
    }
  }

}