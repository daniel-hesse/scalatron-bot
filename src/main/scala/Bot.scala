// mjambach bot
import scala.util.control.NonFatal
import util.Random

class ControlFunction {
  var Count = 90
  var X = 1
  var Y = 0
  val rnd = new Random()
  var dir = ""
  var x = 0
  var y = 0
  var steps = 0

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

      //Master Node
      if (generation == 0) {
        val energy = paramMap("energy").toInt
        val collision = paramMap.getOrElse("collision", "")
        val slaveCount = paramMap("slaves").toInt
        val viewString = paramMap.getOrElse("view", "")
        val view = View(viewString)

        checkDirection(view)

        command = s"Move(direction=$dir)" +: command
        steps = steps + 1

        if (energy >= 100 && slaveCount <= 30) {
          val botDir = XY(dir).negate
          command = s"Spawn(direction=$botDir,energy=100,heading=$botDir)" +: command
        }

      } else {
        //Slave
        val master = paramMap("master")
        val heading = paramMap("heading")
        val viewString = paramMap.getOrElse("view", "")
        val view = View(viewString)

        checkDirection(view)

        command = s"Move(direction=$dir)" +: command
      }

      command.mkString("|")
    } else {
      "" // response if false
    }
  }

  def checkDirection(view: View) = {
    view.offsetToNearest('B').foreach (offset => dir = offset.signum.toString)
    view.offsetToNearest('P').foreach (offset => dir = offset.signum.toString)

    //Check direction
    if (dir == "") { generateDirection(view) }
    var viewdir = view.cellAtRelPos(XY(dir))

    if (viewdir != '_' && viewdir != 'P' && viewdir != 'B') {
      generateDirection(view)
    }
  }

  def generateDirection(view: View) = {
    import XY._

    var possibilities = List[XY](Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight)

    val goodPossibilities = possibilities.filter(p=> view.cellAtRelPos(p)=='_' || view.cellAtRelPos(p)=='P' || view.cellAtRelPos(p)=='B')

    if (goodPossibilities.isEmpty) {
      val random = rnd.nextInt(possibilities.length)
      dir = possibilities(random).toString
    } else {
      val random = rnd.nextInt(goodPossibilities.length)
      dir = goodPossibilities(random).toString
    }
  }
}


class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}