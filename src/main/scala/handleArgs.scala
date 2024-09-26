class HandleArgs {
  def handleArgs(args: Array[String]): List[String] = {
    val defaults = Map(
      "-c" -> "10",           // Default cloud-size
      "--cloud-size" -> "10", // Default cloud-size for long option
      "-l" -> "6",            // Default length-at-least
      "--length-at-least" -> "6", // Default length-at-least for long option
      "-w" -> "1000",         // Default window-size
      "--window-size" -> "1000" // Default window-size for long option
    )

    val parsedArgs = args.grouped(2).collect {
      case Array("-c", value) | Array("--cloud-size", value) => s"cloud-size: $value"
      case Array("-l", value) | Array("--length-at-least", value) => s"length-at-least: $value"
      case Array("-w", value) | Array("--window-size", value) => s"window-size: $value"
    }.toList

    // Use defaults if not specified
    val cloudSize = parsedArgs.find(_.startsWith("cloud-size")).getOrElse(s"cloud-size: ${defaults("-c")}")
    val lengthAtLeast = parsedArgs.find(_.startsWith("length-at-least")).getOrElse(s"length-at-least: ${defaults("-l")}")
    val windowSize = parsedArgs.find(_.startsWith("window-size")).getOrElse(s"window-size: ${defaults("-w")}")

    List(cloudSize, lengthAtLeast, windowSize)
  }
}