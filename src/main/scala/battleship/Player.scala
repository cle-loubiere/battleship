package battleship

trait Player{

    /**
    *   Ask the player for coordinate x,y
    *   @param gameState a GameState
    *   @return two int x and y
    */
    def askShootCoordinate(gameState : GameState):(Int,Int)

    /**
    *   Ask the player for the ship's data coordinate and direction
    *   @param gameState a GameState
    *   @return a ship
    */
    def askShip(size:Int):Ship
}