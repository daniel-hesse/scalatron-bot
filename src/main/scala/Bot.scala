// mjambach bot
import util.Random

class ControlFunction {
  val rnd = new Random()
  val collectionModeStart = 150
  var maxSlaves = 0
  var roundTime = 0

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
      val generation = paramMap("generation").toInt
      val timeRemaining = roundTime - paramMap("time").toInt
      val energy = paramMap("energy").toInt
      val collision = paramMap.getOrElse("collision", "")
      val slaveCount = paramMap("slaves").toInt
      val viewString = paramMap.getOrElse("view", "")
      val view = View(viewString)
      var dir = paramMap.getOrElse("currentdir", "1:1").toString

      //Master Node
      if (generation == 0) {

        // Debug output of view
        if (dir != "") {
          var viewdir = view.cellAtRelPos(XY(dir))
          command = s"Status(text=$viewdir)" +: command
        }

        // Check current direction and make some decisions
        dir = checkDirection(dir, view, generation, slaveCount, timeRemaining, "0:0")
        command = s"Move(direction=$dir)" +: command

        if (energy >= 100 && slaveCount <= 10 && timeRemaining >= collectionModeStart) {
          val botDir = XY(dir).negate
          command = s"Spawn(direction=$botDir,energy=100,heading=$botDir)" +: command
        }

        // Enhancement "save current direction"
        command = s"Set(currentdir=$dir)" +: command
      } else {
        //Slave
        var masterDir = paramMap("master")
        var botName = paramMap("name")
        masterDir = XY(masterDir).signum.toString


        // Enhance this:
        // - Should explode if enemy is two stepps away instead of one
        // - Should be function like checkEnemies

        if (slaveCount >= 20 && (view.cellAtRelPos(XY(dir)) == 'm' || view.cellAtRelPos(XY(dir)) == 's') ) {
          command = s"Explode(size=4)" +: command
        }

        dir = checkDirection(dir, view, generation, slaveCount, timeRemaining, masterDir)

        if (slaveCount >= 20 && (view.cellAtRelPos(XY(dir)) == 'm' || view.cellAtRelPos(XY(dir)) == 's')) {
          command = s"Explode(size=4)" +: command
        }

        // If energy is enough, slaveCount below 100 and enough time, spawn new bots
        if (energy >= 100 && slaveCount <= 300 && timeRemaining >= collectionModeStart) {
          val botDir = XY(dir).negate
          command = s"Spawn(direction=$botDir,energy=100,heading=$botDir)" +: command
        }

        command = s"Move(direction=$dir)" +: command

        // Enhancement "save current direction"
        command = s"Set(currentdir=$dir)" +: command
      }

      // Send command to server
      command.mkString("|")
    } else if (opcode == "Welcome") {
      roundTime = paramMap("apocalypse").toInt
      maxSlaves = paramMap.getOrElse("maxslaves", "300" ).toInt

      command = s"Move(direction=1:1)" +: command
      command = s"Spawn(direction=-1:-1,energy=100,heading=-1:-1)" +: command
      command = s"Set(currentdir=1:1)" +: command
      // Send command to server
      command.mkString("|")
    } else {
      ""
    }
  }


  def checkDirection(curdir: String, view: View, generation: Int, slaveCount: Int, timeRemaining: Int, masterDir: String): String = {
    var dir = curdir
    var poffsetsteps = 1000
    var boffsetsteps = 1000
    var poffsetdir = ""
    var boffsetdir = ""

    // Fix for empty dir
    if (dir == "") { var dir = generateDirection(view) }

    if (timeRemaining >= collectionModeStart) {
      // Check direction
      // Master should just eat static plants
      // Gen1 slaves can eat plants and good beasts but should not die
      // Gen2+ slaves (with low energy) can eat plants, good plants and enemy masters/slaves
      // Gen2+ slaves with high energy should prefer own Master to give energy back
      if (generation == 0) { // Master
        view.offsetToNearest('P').foreach(offset => dir = offset.signum.toString)

        var viewdir = view.cellAtRelPos(XY(dir))

        if (viewdir != '_' && viewdir != 'P' && viewdir != 'B' && viewdir != 'S') { dir = generateDirection(view) }
      } else if (generation >= 1 && slaveCount < 40) {
          view.offsetToNearest('P').foreach (offset => {
            poffsetsteps = offset.stepCount
            poffsetdir=offset.signum.toString
          })

          view.offsetToNearest('B').foreach (offset => {
            boffsetsteps = offset.stepCount
            boffsetdir=offset.signum.toString
          })

        if (boffsetsteps < poffsetsteps) { // beast is next
          dir = boffsetdir
        } else if (boffsetsteps > poffsetsteps) { // plant is next
          dir = poffsetdir
        } else if (poffsetsteps != 1000 && boffsetsteps != 1000 && boffsetsteps == poffsetsteps) { // beast and plant next to us
          dir = boffsetdir
        } else { // nothing there
          // dir stays the same
        }

        var viewdir = view.cellAtRelPos(XY(dir))
        if (viewdir != '_' && viewdir != 'P' && viewdir != 'B') { dir = generateDirection(view) }
      } else {
          view.offsetToNearest('m').foreach (offset => dir = offset.signum.toString)
          view.offsetToNearest('P').foreach (offset => {
            poffsetsteps = offset.stepCount
            poffsetdir=offset.signum.toString
          })

          view.offsetToNearest('B').foreach (offset => {
            boffsetsteps = offset.stepCount
            boffsetdir=offset.signum.toString
          })

        if (boffsetsteps < poffsetsteps) { // beast is next
          dir = boffsetdir
        } else if (boffsetsteps > poffsetsteps) { // plant is next
          dir = poffsetdir
        } else if (poffsetsteps != 1000 && boffsetsteps != 1000 && boffsetsteps == poffsetsteps) { // beast and plant next to us
          dir = boffsetdir
        } else { // nothing there
          // dir stays the same
        }

          view.offsetToNearest('s').foreach (offset => dir = offset.signum.toString)
          var viewdir = view.cellAtRelPos(XY(dir))
          if (viewdir != '_' && viewdir != 'P' && viewdir != 'B' && viewdir != 'm' && viewdir != 's') { dir = generateDirection(view) }
      }
    } else {
      // If time is nearly over, move everything to master
      if (generation == 0) {
        view.offsetToNearest('S').foreach(offset => dir = offset.signum.toString)
      } else {
        dir = masterDir
      }

      //Check direction
      var viewdir = view.cellAtRelPos(XY(dir))

      // Check if something bad is in the new way
      // Masters and gen1 slaves likes only empty room, plants and good beasts
      // Slaves gen2+ like enemy master and slaves too
      if (viewdir != '_' && viewdir != 'P' && viewdir != 'B' && viewdir != 'S' && viewdir != 'M') {
        dir = generateDirection(view)
      }
    }

    dir
  }

  def generateDirection(view: View): String = {
    import XY._
    var dir = ""

    val possibilities = List[XY](Right, RightUp, Up, UpLeft, Left, LeftDown, Down, DownRight)
    val goodPossibilities = possibilities.filter(p=> view.cellAtRelPos(p)=='_' || view.cellAtRelPos(p)=='P' || view.cellAtRelPos(p)=='B')

    if (goodPossibilities.isEmpty) {
      var random = rnd.nextInt(possibilities.length)
      dir = possibilities(random).toString
    } else {
      var random = rnd.nextInt(goodPossibilities.length)
      dir = goodPossibilities(random).toString
    }

    dir
  }
}


class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}