package battleship
import scala.annotation.tailrec

class Human extends Player{
    /**
    *   Ask the player for coordinate x,y
    *   @param actionList a ActionList
    *   @return two int x and y
    */
    @tailrec
    final def  askShootCoordinate(actionList : ActionList):(Int,Int)={
        println("Type the coordinates of your next shot :\n Coornodinates pattern is 'letterNumber' ex : a1 ")
        val coordinate = scala.io.StdIn.readLine().toUpperCase
        if(validCoordinate(coordinate)){
            (coordinate.slice(1,coordinate.length).toInt-1, coordinate.charAt(0).toInt - 64 -1)
        }else askShootCoordinate(actionList)
    }

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param actionList a ActionList
    *   @return a ship
    */
    @tailrec
    final def  askShip(size:Int):Ship={
        println("Type the coordinates of your ship of size " + size  +":\n Coornodinates pattern is 'letterNumber' ex : a1 ")
        val coordinate = scala.io.StdIn.readLine().toUpperCase
        println("Type the orientation of your ship of size " + size  +":\n correct orientation are w, s, e and n  which stand for (w)est, (s)outh, e(ast) and n(orth) ")
        val orientation = scala.io.StdIn.readLine().toLowerCase
        if(validCoordinate(coordinate) && validOrientation(orientation)){
            new Ship(List.fill(size)(false),orientation.charAt(0),coordinate.slice(1,coordinate.length).toInt,coordinate.charAt(0).toInt - 64)
        } else askShip(size)
    }

    /**
    *   Check if the coordinates entered by the user are exact
    *   @param coordinate the coordinates entered by the user
    *   @return a boolean
    */
    private def validCoordinate(coordinate:String):Boolean={
        if(coordinate.length == 0)false
        else{
            val firstPart = coordinate.charAt(0)
            val secondPart = coordinate.slice(1,coordinate.length)
            if(!isAllDigits(secondPart) || firstPart<=64 ||  firstPart>=91) false
            else true
        }    
    }

    /**
    *   Check if a String is only composed of digits
    *   @param string the String to check
    *   @return a Boolean
    */
    private def isAllDigits(string : String):Boolean={
        string.matches("^\\d+$")
    }

    /**
    *   Check if a string is a valid orientation (w,n,e or s)
    *   @param orientation the string to check
    *   @return a Boolean
    */
    private def validOrientation(orientation:String):Boolean={
        if ((orientation.length ==1  )&& ((orientation.charAt(0) == ('w') || (orientation.charAt(0)) ==  ('e') || 
        (orientation.charAt(0)) == ('s') || (orientation.charAt(0)) == ('n')))) true
        else false
    }
}