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

object CommandParser {
    def apply(command: String) = {
        def splitParam(param: String) = {
            val segments = param.split('=')
            if( segments.length != 2 )
                throw new IllegalStateException("invalid key/value pair: " + param)
            (segments(0),segments(1))
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)

        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map( splitParam ).toMap
        (segments(0), keyValuePairs)
    }
}

case class View(cells: String) {
    val size = math.sqrt(cells.length).intValue
    val center = XY(size / 2, size / 2)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def apply(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))
}

case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x != 0 || y!=0
    def isZero = x == 0 && y == 0
    def isNonNegative = x >= 0 && y >= 0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x+dx, y)
    def addToY(dy: Int) = XY(x, y+dy)

    def +(pos: XY) = XY(x+pos.x, y+pos.y)
    def -(pos: XY) = XY(x-pos.x, y-pos.y)
    def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)

    def distanceTo(pos: XY) : Double = (this-pos).length
    def length : Double = math.sqrt(x*x + y*y)

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)
}

object XY {
    // Get X and Y from direction string for example "1:0"
    def apply(s: String): XY = {
        val a = s.split(':'); (a(0).toInt, a(1).toInt)
    }

    val Zero = XY(0, 0)
    val One = XY(1, 1)

    val Right = XY(1, 0)
    val RightUp = XY(1, -1)
    val Up = XY(0, -1)
    val UpLeft = XY(-1, -1)
    val Left = XY(-1, 0)
    val LeftDown = XY(-1, 1)
    val Down = XY(0, 1)
    val DownRight = XY(1, 1)
}

class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}