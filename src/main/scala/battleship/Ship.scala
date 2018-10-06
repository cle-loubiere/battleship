package battleship

/**
*   A ship
*   @constructor create a ship with an orientation, a position and a list of shooted Zone
*   @param hitList state of each zone, hit or not hit
*   @param orientation the orientation of the ship n(orth),s(outh),e(ast),w(est)
*   @param coordinateX the x coordinate of the ship's origin
*   @param coordinateY the y coordinate of the ship's origin
*/
case class Ship(val hitList:List[Boolean], orientation:Char, coordinateX:Int, coordinateY:Int ) {
    /**
    * The length of the ship
    */
    val length = hitList.length

    /**
    *   Normalization of the ship's orientation, can be s(outh) or e(ast)
    *   direction reduce the number of case from 4 to 2
    */
    val direction = {
        if(orientation == 'n') 's'
        else if(orientation == 'w') 'e'
        else orientation
    }

    /**
    *   new origin x of the ship after the orientation's normalization
    */
    val posX = {
        if(orientation == 'w') coordinateX - length +1
        else coordinateX
    }

    /**
    *   new origin y of the ship after the orientation's normalization
    */
    val posY ={
        if(orientation == 'n') coordinateY - length +1
        else coordinateY
    }
    
    /**
    *   know if the position (x,y) is occuped by the ship
    *   
    *   @param x the x coordinate of the position
    *   @param y the y coordinate of the position 
    *   @return true if the position (x,y) is occuped by the ship 
    */
    def isInCoordinate(x: Int, y:Int):Boolean = {
        if(direction == 'e'){
             (posX <= x && (posX+length) > x && posY == y)
        }else{
             (posY <= y && (posY+length) > y && posX == x)
        }
    }

    /**
    *   know if a shot at the position (x,y) damage the ship
    *   
    *   @param x the x coordinate of the shot's position
    *   @param y the y coordinate of the shot's position 
    *   @return true if the position (x,y) is occuped by the ship 
    *            and have not been damage 
    */
    def isShootable(x:Int, y:Int):Boolean ={
        if( this.isInCoordinate(x,y) ){
            if(direction == 'e'){
             !hitList(posX-x)
            }else{
             !hitList(posY-y)
            }
        }else false
    }

    /**
    *   know if the ship is sunk
    *   
    *   @return true if the ship is sunk (all of its zone have been hit), 
    */
    def isSunk():Boolean = {
        //hitList.forall(x => x==true)
        !hitList.contains(false)
    }

    /**
    *   Create a ship reprensenting the state of the boat
    * after a shot occured at pos(x,y)
    *   
    *   @param x the x coordinate of the position shooted
    *   @param y the y coordinate of the position shooted
    *   @return this ship after the shot
    */
    def shoot(x:Int, y:Int):Ship = {
        if(this.isInCoordinate(x,y)){
            if((x-posX) > 0){
                val newHitList = hitList.updated(x-posX,true)
                this.copy(hitList = newHitList)
            }else{
                val newHitList = hitList.updated(y-posY,true)
                this.copy(hitList = newHitList)
            }
            
        }else this
    }

    /**
    *   know if this ship and another one share a coordinate
    *   
    *   @param ship the ship for comparison
    *   @return true if the ships share a coordinate 
    */
    def collision(ship:Ship):Boolean={
        if(direction == ship.direction){
            if(direction == 'e'){
                ((posY == ship.posY)
                && (((posX <= ship.posX) && (posX+length-1 >= ship.posX))
                    ||((ship.posX <= posX)&&( ship.posX+ship.length-1 >= posX)))) 
            }else{
                ((posX == ship.posX)
                && (((posY <= ship.posY) && (posY+length-1 >= ship.posY))
                    ||((ship.posY <= posY)&&( ship.posY+ship.length-1 >= posY))))  
            }
        }else {
            if(direction == 'e'){
                (((posX <= ship.posX) && (posX+length-1>=ship.posX)) 
                && ((ship.posY <= posY)&&(ship.posY + ship.length-1>=posY)))
            }else{
                (((ship.posX <= posX) && (ship.posX+ship.length-1>=posX)) 
                && ((posY <= ship.posY)&&(posY + length-1>=ship.posY)))
            }
        }
    }

    /**
    *   know if this ship and another one share a coordinate
    *   
    *   @param heigth the heigth of a grid
    *   @param length the length of a grid
    *   @return true if the ship is out of bounds
    */
    def isOutOfBounds(heigth:Int,length:Int):Boolean={
        if(this.direction == 'e'){
            if(this.posY > heigth || this.posX + this.length -1 > length || posX < 1 || posY < 1){
                true
            }else false
        }else{
            if(this.posX > length || this.posY + this.length -1 > heigth || posX < 1 || posY < 1){
                true
            }else false
        }

    }
}