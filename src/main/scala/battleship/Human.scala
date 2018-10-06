package battleship
import scala.annotation.tailrec

class Human extends Player{
    /**
    *   Ask the player for coordinate x,y
    *   @param gameState a GameState
    *   @return two int x and y
    */
    @tailrec
    final def  askShootCoordinate(gameState : GameState):(Int,Int)={
        println("Type the coordinates of your next shot :\n Coornodinates pattern is 'letterNumber' ex : a1 ")
        val coordinate = scala.io.StdIn.readLine().toUpperCase
        if(validCoordinate(coordinate)){
            (coordinate.slice(1,coordinate.length).toInt, coordinate.charAt(0).toInt - 64)
        }else askShootCoordinate(gameState)
    }

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param gameState a GameState
    *   @return a ship
    */
    @tailrec
    final def  askShip(size:Int):Ship={
        println("Type the coordinates of your ship of size " + size  +":\n Coornodinates pattern is 'letterNumber' ex : a1 ")
        val coordinate = scala.io.StdIn.readLine().toUpperCase
        println("Type the orientation of your ship of size " + size  +":\n correct orientation are w, s, e and n  which stand for (w)est, (s)outh, e(ast) and n(orth) ")
        val orientation = scala.io.StdIn.readLine().toLowerCase
        if(validCoordinate(coordinate) && validOrientation(orientation)){
            return new Ship(List.fill(size)(false),orientation.charAt(0),coordinate.slice(1,coordinate.length).toInt,coordinate.charAt(0).toInt - 64)
        } else askShip(size)
    }

    private def validCoordinate(coordinate:String):Boolean={
        
        val firstPart = coordinate.charAt(0).toInt
        true
        //val secondPart = coordinate.slice(1,coordinate.length).toInt
       /* val intRegex = """(\d+)""".r
        val isInt = intRegex(coordinate.slice(1,coordinate.length))*/
        /*(coordinate.slice(1,coordinate.length))match{
            case (i: Int)=>{
                if( firstPart>64 &&  firstPart<91)true
                else false
            }
            case _ =>{
                false
            }
        }*/
            
    }

    private def validOrientation(orientation:String):Boolean={
        if ((orientation.length ==1  )&& ((orientation.charAt(0) == ('w') || (orientation.charAt(0)) ==  ('e') || 
        (orientation.charAt(0)) == ('s') || (orientation.charAt(0)) == ('n')))) true
        else false
    }
}