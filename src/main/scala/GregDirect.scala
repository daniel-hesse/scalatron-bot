// Tutorial Bot #1: Hello World

class ControlFunction {
    
    var a = 0
    var b = 0
    var n = 0
def respond(input: String) = {
    val tokens = input.split('(')
    val r = scala.util.Random // split at '(', returns Array[String
    var count = r.nextInt(100)
    if(tokens(0)=="React") {        // token(0): 0th element of array
        val rest = tokens(1).dropRight(1)               // "key=value,key=value,key=value"
        val params = rest.split(',')                    // = Array("key=value", "key=value", ...)
        val strPairs = params.map(s => s.split('='))    // = Array( Array("key","value"), Array("key","value"), ..)
        val kvPairs = strPairs.map(a => (a(0),a(1)))    // = Array( ("key","value"), ("key","value"), ..)
        val paramMap = kvPairs.toMap
        
       // if (paramMap.contains("collision"))
        
        
        
        if (n > count || paramMap.contains("collision")){
            a = r.nextInt(10) -5
            b = r.nextInt(10) -5
            if (n > count){n = 0}
        }
        n += 1
        "Say(text=wush! )|Move(direction="+a+":"+b+")|Spawn(direction="+a+":"+b+",energy=100)"
        
        // response if true
    } else {
        n += 1
        "Say(text=Energy:  )|Move(direction=-1:0)|Spawn(direction=1:1,energy=100,role=missile)"
                                 // response if false
    }
    
}
}


class ControlFunctionFactory {
    def create = new ControlFunction().respond _
}

