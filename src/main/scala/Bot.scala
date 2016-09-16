// mjambach bot
import util.Random

class ControlFunction {
    var Count = 20
    var X = 1
    var Y = 0
    val rnd = new Random()
    val range = 1 to 8
    var RANDOM = 0
    var dir = ""
    var x = 0
    var y = 0

    def respond(input: String) = {
        val (opcode, paramMap) = CommandParser(input) //parseResult = parse(input)
        var command = ""


        if(opcode=="React") { // token(0): 0th element of array
        val generation = paramMap("generation").toInt
            if (generation == 0) {
                val energy = paramMap("energy").toInt
                val collision = paramMap.getOrElse("collision","")
                val viewString = paramMap.getOrElse("view","")
                val view = View(viewString)

                if((dir == "") || collision != "") {
                    var (x, y, dir) = generateDirection()
                } else {
                    //ReDefine Direction
                    //if (view(XY(x, y)) != '_') {
                    //    var (x, y, dir) = generateDirection()
                    //}
                }

                command += "Move(direction=" + dir + ")"


                if (energy >= 100) {
                    command += "|Spawn(direction=" + dir + ",energy=100,heading=" + dir + ")"
                }

                command += "|Status(text=" + XY(dir).toString + ")"

            } else {
                //Slave
                val master = paramMap("master")
                val heading = paramMap("heading")

                command += "Move(direction=" + heading + ")"
            }
            command     // response if true
        } else {
            ""                          // response if false
        }
    }

    def generateDirection() = {
        RANDOM = range(rnd.nextInt(range length))
        if (RANDOM == 1 ) { x = 1; y = 1; dir = x.toString + ":" + y.toString }
        if (RANDOM == 2 ) { x = 1; y = 0; dir = x.toString + ":" + y.toString }
        if (RANDOM == 3 ) { x = 1; y = -1; dir = x.toString + ":" + y.toString }
        if (RANDOM == 4 ) { x = 0; y = -1; dir = x.toString + ":" + y.toString }
        if (RANDOM == 5 ) { x = 0; y = 1; dir = x.toString + ":" + y.toString }
        if (RANDOM == 6 ) { x = -1; y = 1; dir = x.toString + ":" + y.toString }
        if (RANDOM == 7 ) { x = -1; y = 0; dir = x.toString + ":" + y.toString }
        if (RANDOM == 8 ) { x = -1; y = -1; dir = x.toString + ":" + y.toString }
        (x, y, dir)
    }
}









class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}