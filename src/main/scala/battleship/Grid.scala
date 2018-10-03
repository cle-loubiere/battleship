package battleship
import scala.annotation.tailrec
/**
*   A grid
*   @constructor create a grid with a length and heigth
*   @param length the length of the grid
*   @param heigth the y coordinate of the ship's origin
*   @param grid optionnal copy the grid
*/
case class Grid(val grid : List[List[String]]){
    def this( heigth : Int,  length : Int) = {
        this(List.fill(heigth)(List.fill(length)("w")))
    }
    val length = grid(0).size
    val heigth = grid.size
    /**
    * Syntactic sugar to access a cell
    */
    def apply(heigth:Int,length:Int):String={
        grid(heigth)(length)
    }

    /**
    * Syntactic sugar to access a List
    */
    def apply(heigth:Int):List[String]={
        grid(heigth)
    }

    /**
    *   create a new Grid of the same size with a different cell
    *   
    *   @param x the x coordinate of the cell to change
    *   @param y the y coordinate of the cell to change
    *   @param newString the String to place in the new cell
    *   @return a new Grid
    */
    def changeGridTile(heigth:Int,length:Int, newString:String):Grid = {
        val newList = grid(heigth).updated(length,newString)
        val newGrid = grid.updated(heigth,newList)
        this.copy(grid = newGrid) 
    }

    /**
    *   create a new Grid with cell containing the location of a ship
    *   
    *   
    *   @param ship the ship to place in the grid
    *   @return a new Grid
    */
    def insertShip(ship:Ship):Grid={
        insertShip(ship,this,0)
    }
    @tailrec
    private def insertShip(ship:Ship,newGrid:Grid,count:Int):Grid={
        if(count == ship.length) newGrid
        else{
            if(ship.direction == 'e'){
                val changedGrid = newGrid.changeGridTile(ship.posY-1,ship.posX-1 + count,"b")
                insertShip(ship,changedGrid,count+1)
            }else{
                val changedGrid = newGrid.changeGridTile(ship.posY-1 + count,ship.posX-1,"b")
                insertShip(ship,changedGrid,count+1)
            }
            
        }
    }
}