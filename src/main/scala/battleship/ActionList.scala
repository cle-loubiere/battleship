package battleship

case class ActionList(val actionList : List[(Int,Int,String)] = List()){
    
    /**
    *   create a new actionlist with a new action
    *   @param x the x coordinate of the new action
    *   @param y the y coordinate of the new action
    *   @param result the result of the new action can be "miss", "hit" or "sunk"
    *   @return a new ActionList
    */
    def addAction(x : Int, y : Int, result: String):ActionList={
        new ActionList(actionList :+ (x,y,result))
    }

    /**
    *   know if there already been a shot at target coordinates
    *   @param x the x of the coordinates
    *   @param y the y of the coordinates
    *   @return a Boolean
    */
    def areCoordinatesAlreadyTargeted(x :Int,y:Int):Boolean={
        if(actionList.map(x => (x._1,x._2)).contains(x,y))true
        else false
    }

    /**
    *   know if the last shot hit or not
    *   @return a Boolean
    */
    def hasLastShotHitAndNotSunk():Boolean={
        if(actionList.last._3 == "hit") true
        else false
    }
}