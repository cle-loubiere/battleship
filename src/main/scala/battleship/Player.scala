package battleship

trait Player{

    /**
    *   Ask the player for coordinate x,y
    *   @param actionList a ActionList
    *   @return two int x and y
    */
    def askShootCoordinate(actionList : ActionList):(Int,Int)

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param actionList a ActionList
    *   @return a ship
    */
    def askShip(size:Int):Ship
}