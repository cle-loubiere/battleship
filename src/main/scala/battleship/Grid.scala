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
                val changedGrid = newGrid.changeGridTile(ship.posY-1,ship.posX-1 + count,"X")
                insertShip(ship,changedGrid,count+1)
            }else{
                val changedGrid = newGrid.changeGridTile(ship.posY-1 + count,ship.posX-1,"X")
                insertShip(ship,changedGrid,count+1)
            }
            
        }
    }

    /**
    *   create a new Grid of the state after a shot
    *   @param x the x coordonnate of the shot
    *   @param y the y coordonnate of the shot
    *   @return a new Grid
    */
    def shoot(y:Int,x:Int):Grid={
         this.changeGridTile(y,x,grid(y)(x)+"_hit")
        
    }

    

    private def createTopGrid():String = {
        createTopGrid("",1)
    }
    @tailrec
    private def createTopGrid(topGrid:String, begin : Int):String = {
        if(begin > this.length) topGrid +"\n"
        else createTopGrid(topGrid +"  " + begin.toString, begin+1)
    }

    /**
    *   create a string representing the state of the grid with only the public informations
    *   @return a String
    */
    def toStringPublicInfo():String={
        toStringPublicInfo(0,0,createTopGrid)
    }

    @tailrec
    private def toStringPublicInfo(x:Int, y:Int, result:String):String={
        if(y>=this.heigth) result
        else{
            if(this(y,x).contains("hit")){
                if(x==0){
                    val newResult = result +(64+y+1).toChar+"|" + this(y,x).charAt(0) + "|"
                    if(x+1 >= this.length){
                        toStringPublicInfo(0,y+1,newResult+"\n"/*+ "_" * 3 * this.length +"\n"*/)
                    }else{
                        toStringPublicInfo(x+1,y,newResult)
                    }
                }else{
                    val newResult = result +"|" + this(y,x).charAt(0) + "|"
                    if(x+1 >= this.length){
                        toStringPublicInfo(0,y+1,newResult+"\n"/*+ "_" * 3 * this.length +"\n"*/)
                    }else{
                        toStringPublicInfo(x+1,y,newResult)
                    }
                }
            }else{
                if(x==0){
                    val newResult = result +(64+y+1).toChar+"|-|"
                    if(x+1 >= this.length){
                    toStringPublicInfo(0,y+1,newResult+"\n"/*+ " " * 3 * this.length +"\n"*/)
                    }else{
                        toStringPublicInfo(x+1,y,newResult)
                    }
                }else{
                    val newResult = result +"|-|"
                    if(x+1 >= this.length){
                    toStringPublicInfo(0,y+1,newResult+"\n"/*+ " " * 3 * this.length +"\n"*/)
                    }else{
                        toStringPublicInfo(x+1,y,newResult)
                    }
                }
              
            }
        }
    }

    /**
    *   create a string representing the state of the grid with all the informations
    *   @return a String
    */
    def toStringPrivateInfo():String={
        toStringPrivateInfo(0,0,createTopGrid)
    }

    @tailrec
    private def toStringPrivateInfo(x:Int, y:Int, result:String):String={
        if(y>=this.heigth) result
        else{
            if(x==0){
                val intermediateResult = result + (64+y+1).toChar
                if(this(y,x).contains("hit")){
                    if(this(y,x).contains("w")){
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|~|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|~|")
                        }
                    }else{
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|X|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|X|")
                        }
                    }
                }else{
                    if(this(y,x).contains("w")){
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|-|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|-|")
                        }
                    }else{
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|S|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|S|")
                        }
                    }
                }
            }else {
                val intermediateResult = result
                if(this(y,x).contains("hit")){
                    if(this(y,x).contains("w")){
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|~|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|~|")
                        }
                    }else{
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|X|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|X|")
                        }
                    }
                }else{
                    if(this(y,x).contains("w")){
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|-|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|-|")
                        }
                    }else{
                        if(x+1 >= this.length){
                            toStringPrivateInfo(0,y+1,intermediateResult+"|S|"+"\n")
                        }else{
                            toStringPrivateInfo(x+1,y,intermediateResult+"|S|")
                        }
                    }
                }
            } 
        }
    }
}