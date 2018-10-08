package battleship
import scala.util.Random
import scala.annotation.tailrec

class HardAI extends Player{
    val Random = new Random()
    /**
    *   Ask the player for coordinate x,y
    *   @param actionList a ActionList
    *   @return two int x and y
    */
    
    final def askShootCoordinate(actionList : ActionList):(Int,Int)={
        
        if(actionList.actionList.length > 0){
            val lastShot = actionList.actionList.last
            if(actionList.hasLastShotHitAndNotSunk && lastShot._1 != 9 && lastShot._1 != 0 && lastShot._2 != 9 && lastShot._2 != 0)
                /*Random.nextInt(4) match {
                    case 0 => if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1-1,lastShot._2)){(lastShot._1-1, lastShot._2)} else randomShot(actionList)
                    case 1 => if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1+1,lastShot._2)){(lastShot._1+1, lastShot._2)} else randomShot(actionList)
                    case 2 => if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1,lastShot._2-1)){(lastShot._1, lastShot._2-1)} else randomShot(actionList)
                    case 3 => if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1,lastShot._2+1)){(lastShot._1, lastShot._2+1)} else randomShot(actionList)
                }*/
                if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1-1,lastShot._2)){(lastShot._1-1, lastShot._2)} 
                else if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1+1,lastShot._2)){(lastShot._1+1, lastShot._2)} 
                else if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1,lastShot._2-1)){(lastShot._1, lastShot._2-1)} 
                else if(!actionList.areCoordinatesAlreadyTargeted(lastShot._1,lastShot._2+1)){(lastShot._1, lastShot._2+1)} 
                else randomShot(actionList)
            else{
                randomShot(actionList)
            }
            

        }else{
            randomShot(actionList)
        }
    }
    @tailrec
    private def randomShot(actionList : ActionList):(Int,Int)={
        val shot = (Random.nextInt(10),Random.nextInt(10))
            if(actionList.areCoordinatesAlreadyTargeted(shot._1,shot._2)) randomShot(actionList)
            else shot
    }

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param actionList a ActionList
    *   @return a ship
    */
    def askShip(size:Int):Ship={
        Random.nextInt(4) match {
            case 0 => new Ship(List.fill(size)(false),'s',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 1 => new Ship(List.fill(size)(false),'w',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 2 => new Ship(List.fill(size)(false),'e',Random.nextInt(9)+1,Random.nextInt(9)+1)
            case 3 => new Ship(List.fill(size)(false),'n',Random.nextInt(9)+1,Random.nextInt(9)+1)
        }
    }
}