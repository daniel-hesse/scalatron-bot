// mjambach bot
import scala.util.control.NonFatal
import util.Random

class ControlFunction {
  var Count = 30
  var X = 1
  var Y = 0
  val rnd = new Random()
  val range = 1 to 8
  var RANDOM = 0
  var dir = ""
  var x = 0
  var y = 0
  var steps = 0
  var trycount = 0


  def respond(input: String): String = try {
    tryrespond(input)
  }
  catch {
    case e: Throwable =>
      e.printStackTrace(); ""
  }

  def tryrespond(input: String) = {
    val (opcode, paramMap) = CommandParser(input) //parseResult = parse(input)
    var command = List[String]()


    if (opcode == "React") {
      // token(0): 0th element of array
      val generation = paramMap("generation").toInt
      if (generation == 0) {
        val energy = paramMap("energy").toInt
        val collision = paramMap.getOrElse("collision", "")
        val viewString = paramMap.getOrElse("view", "")
        val view = View(viewString)
        var viewdir = ""
        if ((dir == "") || collision != "" || steps > Count) {
          generateDirection()
        } else {
          //Check direction
          viewdir = view.cellAtRelPos(XY(dir)).toString()
          command = s"Status(text=$viewdir)" +: command
          trycount = 0
          while (viewdir != "_" && viewdir != "P" && viewdir != "B" && trycount < 10 ) {
             trycount = trycount + 1
             generateDirection()
             viewdir = view.cellAtRelPos(XY(dir)).toString()
          }


        }

        command = s"Move(direction=$dir)" +: command
        steps = steps + 1

        if (energy >= 100) {
          val botDir = XY(dir).negate
          command = s"Spawn(direction=$botDir,energy=100,heading=$botDir)" +: command
        }

      } else {
        //Slave
        val master = paramMap("master")
        val heading = paramMap("heading")

        command = s"Move(direction=$heading)" +: command
      }

      command.mkString("|")
    } else {
      "" // response if false
    }
  }

  def generateDirection() = {
    RANDOM = range(rnd.nextInt(range length))
    steps = 0
    if (RANDOM == 1) {
      x = 1; y = 1; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 2) {
      x = 1; y = 0; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 3) {
      x = 1; y = -1; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 4) {
      x = 0; y = -1; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 5) {
      x = 0; y = 1; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 6) {
      x = -1; y = 1; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 7) {
      x = -1; y = 0; dir = x.toString + ":" + y.toString
    }
    if (RANDOM == 8) {
      x = -1; y = -1; dir = x.toString + ":" + y.toString
    }
  }
}


class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}