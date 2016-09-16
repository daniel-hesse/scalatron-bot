class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      "Status(text=Hello World!)"
    } else ""
  }
}