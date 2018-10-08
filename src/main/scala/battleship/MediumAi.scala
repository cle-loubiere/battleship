package battleship
import scala.util.Random
import scala.annotation.tailrec

class MediumAI extends Player{
    val Random = new Random()
    /**
    *   Ask the player for coordinate x,y
    *   @param actionList a ActionList
    *   @return two int x and y
    */
    @tailrec
    final def askShootCoordinate(actionList : ActionList):(Int,Int)={
        val shot = (Random.nextInt(10),Random.nextInt(10))
        if(actionList.areCoordinatesAlreadyTargeted(shot._1,shot._2)) askShootCoordinate(actionList)
        else shot
    }

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param actionList a ActionList
    *   @return a ship
    */
    def askShip(size:Int):Ship={
        Random.nextInt(3) match {
            case 0 => new Ship(List.fill(size)(false),'s',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 1 => new Ship(List.fill(size)(false),'w',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 2 => new Ship(List.fill(size)(false),'e',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 3 => new Ship(List.fill(size)(false),'n',Random.nextInt(9)+1,Random.nextInt(9)+1)
        }
    }
}