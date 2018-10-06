package battleship

case class GameState(val Actionlist : List[((Int,Int),String)],val shipList : List[Ship]){
    def this(shipList:List[Ship]){
        this( List(), shipList)
    }
}