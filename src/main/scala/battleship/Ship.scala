package battleship

case class Ship(val hitList:List[Boolean], orientation:Char, posX:Int, posY:Int ) {
    
    val length = hitList.length
    val direction = {
        if(orientation == 'n') 's'
        else if(orientation == 'w') 'e'
        else orientation
    } 
    val coordinateX = {
        if(orientation == 'w') posX - length +1
        else posX
    }
    val coordinateY ={
        if(orientation == 'n') posY - length +1
        else posY
    }
    //val hitList = List.fill(length)(false)
   
    def isInCoordinate(x: Int, y:Int):Boolean = {
        if(direction == 'e'){
             (posX <= x && (posX+length) > x && posY == y)
        }else{
             (posY <= y && (posY+length) > y && posX == x)
        }
    }

    def isSunk():Boolean = {
        hitList.forall(x => x==true)
    }

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
}