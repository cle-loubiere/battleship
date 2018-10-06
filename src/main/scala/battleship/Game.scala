package battleship
import scala.annotation.tailrec

object game extends App{
    println("\033c")
    val ConstShipSize = List(2,3,3,4,5)
    //println(new Ship(List.fill(2)(false),'e',1,1).collision(new Ship(List.fill(3)(false),'s',2,1)))
    
    val gameType = chooseGameType
    val players = getPlayers(gameType)
    startGame(players)
    

    def startGame(playerList : List[Player], score : List[Int] = List.fill(2)(0), firstPlayerNumber : Int = 0):Unit={
        //val playersGrid = List.fill(playerList)(new Grid(10,10))
        val playersShips = getPlayersShips(playerList,5)

        //Pseudo code
        // startGame(playerList, NOUVEAUSCORE, firstPlayerNumber+1%playerList.length)
    }

    @tailrec
    def chooseGameType():Int={
        println("Choose the number of your game type\n 1 - Human vs Human \n 2 - Humain vs AI \n 3 - AI vs AI")
        val gameType = scala.io.StdIn.readLine()
        if(gameType.length == 1 && (gameType.contains("1") || gameType.contains("2") || gameType.contains("3"))){
            println("\033c")
            gameType.toInt
        }else{
            println("\033c")
            println("Error : Wrong game type")
            chooseGameType
        }
    }

    @tailrec
    def chooseAIDifficulty():Player={
        println("Choose the difficulty of your AI \n 1 - Easy \n 2 - Medium \n 3 - Hard")
        val aIDifficulty = scala.io.StdIn.readLine()
        if(aIDifficulty.length == 1 && (aIDifficulty.contains("1") || aIDifficulty.contains("2") || aIDifficulty.contains("3"))){
            println("\033c")
            aIDifficulty.toInt match{
                case 1 => {
                    new EasyAI
                }
                case 2 => {
                    new MediumAI
                }
                case 3 => {
                    new HardAI
                }
            }
        }else{
            println("\033c")
            println("Error : Wrong AI type")
            chooseAIDifficulty
        }
    }

    def getPlayers(gameType:Int):/*(Player,Player)*/List[Player]={
        gameType match{
            case 1 => {
                List(new Human, new Human)
            }

            case 2 => {
                List(new Human,chooseAIDifficulty)
            }

            case 3 =>{
                List(chooseAIDifficulty,chooseAIDifficulty)
            }
        }
    }

    def getPlayersShips(playerList:List[Player], numberOfShip: Int):List[List[Ship]]={
        //getPlayerShip(playerList,number,0)
        getPlayersShips(playerList,numberOfShip,0)
    }

    @tailrec
    def getPlayersShips(playerList:List[Player], numberOfShip: Int, cpt:Int, finalList:List[List[Ship]] = List()):List[List[Ship]]={
        //getPlayerShip(playerList,number,0)
        if(cpt == playerList.length)finalList
        else{
            getPlayersShips(playerList,numberOfShip,cpt+1,finalList :+ getPlayerShips(playerList(cpt),numberOfShip))
        }
    }


    def getPlayerShips(player:Player, numberOfShip: Int):List[Ship]={
        getPlayerShips(player,5,List())
    }

    @tailrec
    def getPlayerShips(player:Player, numberOfShip : Int, shipList:List[Ship]):List[Ship]={
        if(numberOfShip == 0) shipList
        else{
            if(player.isInstanceOf[Human]){
                println(new Grid(10,10).insertShipList(shipList).toStringPrivateInfo)
            }
            val newShip = player.askShip(ConstShipSize(5-numberOfShip))
            if(shipList.map(x => x.collision(newShip)).contains(true)){
                println("ship coordinates are in contact with another ship")
                getPlayerShips(player, numberOfShip, shipList )
            }else if(newShip.isOutOfBounds(10,10)){
                println("ship coordinates are out of bound limit is 10,10")
                getPlayerShips(player, numberOfShip, shipList )
            }else{
                getPlayerShips(player, numberOfShip-1, shipList :+ newShip )
            }
        }
    }
}

