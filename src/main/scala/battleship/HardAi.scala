package battleship
import scala.util.Random

class HardAI extends Player{
    val Random = new Random()
    /**
    *   Ask the player for coordinate x,y
    *   @param actionList a ActionList
    *   @return two int x and y
    */
    def askShootCoordinate(actionList : ActionList):(Int,Int)={
        (Random.nextInt(10),Random.nextInt(10))
    }

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param actionList a ActionList
    *   @return a ship
    */
    def askShip(size:Int):Ship={
        Random.nextInt(3) match {
            case 0 => new Ship(List.fill(size)(false),'s',Random.nextInt(10),Random.nextInt(10))
            case 1 => new Ship(List.fill(size)(false),'w',Random.nextInt(10),Random.nextInt(10))
            case 2 => new Ship(List.fill(size)(false),'e',Random.nextInt(10),Random.nextInt(10))
            case 3 => new Ship(List.fill(size)(false),'n',Random.nextInt(10),Random.nextInt(10))
        }
    }
}