package battleship
import scala.annotation.tailrec
import java.io.File
import java.io.PrintWriter

object game extends App{
    println("\033c")
    val ConstShipSize = List(2,3,3,4,5)
    //println(new Ship(List.fill(2)(false),'e',1,1).collision(new Ship(List.fill(3)(false),'s',2,1)))
    
    val gameType = chooseGameType
    if (gameType != 3){
        val players = getPlayers(gameType)
        startGame(players)
    }else{
        val EvM = startGame(List(new EasyAI,new MediumAI))
        val EvH = startGame(List(new EasyAI,new HardAI))
        val MvH = startGame(List(new MediumAI,new HardAI))
        val writer = new PrintWriter(new File("ai_proof.csv"))
        writer.write("AI Name;score;AI Name 2;score2\n")
        writer.write("AI Level Easy;"+EvM(0)+";AI Level Medium;"+EvM(1)+"\n")
        writer.write("AI Level Easy;"+EvH(0)+";AI Level Hard;"+EvH(1)+"\n")
        writer.write("AI Level Medium;"+MvH(0)+";AI Level Hard;"+MvH(1)+"\n")
        writer.close
    }
    
    @tailrec
    def startGame(playerList : List[Player], score : List[Int] = List.fill(2)(0), firstPlayerNumber : Int = 0):List[Int]={
        //val playersGrid = List.fill(playerList)(new Grid(10,10))
        val playersShips = getPlayersShips(playerList,5)
        
        val playerGrid = getPlayersGrid(playersShips)
        val playersActionList = getPlayersActionGrid(playerList)
        
        val winnerNumber = playTheGame(playerList, playerGrid, playersShips, playersActionList,firstPlayerNumber)
        if((gameType != 3)){
            println("type y to play another game")
            if ((scala.io.StdIn.readLine() == "y")){
                startGame(playerList, score.updated(winnerNumber,score(winnerNumber)+1), (firstPlayerNumber+1)%playerList.length)
            }else { 
                println("Score = player 1 : " + score(0) + " victory - player 2 : " + score(1) + " victory")
                score
            }
        }
        else if( ((score(0)+score(1) < 100) && gameType == 3)  ){
            startGame(playerList, score.updated(winnerNumber,score(winnerNumber)+1), (firstPlayerNumber+1)%playerList.length)
        }else { 
            println("Score = player 1 : " + score(0) + " victory - player 2 : " + score(1) + " victory")
            score
        }

    }

    @tailrec
    def playTheGame(playerList : List[Player], playerGridList : List[Grid], playerShipsList : List[List[Ship]], playerActionList : List[ActionList], playerNumber : Int):Int={
        val playerTurn = playerNumber % playerList.length
        val nonActivePlayerNumber = (playerNumber +1) % playerList.length
        val activePlayer = playerList(playerTurn)
        if(activePlayer.isInstanceOf[Human]){
            println("your grid : \n" + playerGridList(playerTurn).toStringPrivateInfo())
            println("your opponent grid : \n" + playerGridList(nonActivePlayerNumber).toStringPublicInfo())
        }
        val shootCoordinates = activePlayer.askShootCoordinate(playerActionList(playerTurn))

        
        val newShipList = playerShipsList.updated(nonActivePlayerNumber, playerShipsList(playerTurn).map(x => x.shoot(shootCoordinates._1+1,shootCoordinates._2+1)))
        val newGridList = playerGridList.updated(nonActivePlayerNumber, playerGridList(nonActivePlayerNumber).shoot(shootCoordinates._2,shootCoordinates._1))
        val shotResult =    if(playerShipsList(nonActivePlayerNumber).map(x => x.isShootable(shootCoordinates._1+1,shootCoordinates._2+1)).contains(true)) "hit"
                            else if(playerShipsList(nonActivePlayerNumber).map(x => x.isSunkable(shootCoordinates._1+1,shootCoordinates._2+1)).contains(true)) "sunk"
                            else "miss"
        if(activePlayer.isInstanceOf[Human]){
            println(shotResult)
        }
        val newPlayerActionList = playerActionList.updated(playerTurn, playerActionList(playerTurn).addAction(shootCoordinates._1,shootCoordinates._2,shotResult))
        if(newShipList(nonActivePlayerNumber).map(x => x.isSunk).contains(false)) playTheGame(playerList,newGridList,newShipList,newPlayerActionList,playerNumber+1)
        else {
            if(activePlayer.isInstanceOf[Human]){
                println("Player " + (playerTurn +1) + " win the game")
            }
            playerTurn
        }
    }

    @tailrec
    def chooseGameType():Int={
        println("Choose the number of your game type\n 1 - Human vs Human \n 2 - Humain vs AI \n 3 - Test AI vs AI")
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
        val test = getPlayerShips(player,5,List())
        if(player.isInstanceOf[Human]){
            println("ship successfully taken")
        }
        test
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
                if(player.isInstanceOf[Human]){
                    println("ship coordinates are in contact with another ship")
                }
                getPlayerShips(player, numberOfShip, shipList )
            }else if(newShip.isOutOfBounds(10,10)){
                if(player.isInstanceOf[Human]){
                    println("ship coordinates are out of bound limit is 10,10")
                }
                getPlayerShips(player, numberOfShip, shipList )
            }else{
                getPlayerShips(player, numberOfShip-1, shipList :+ newShip )
            }
        }
    }

    def getPlayersGrid(playersShipsList : List[List[Ship]]):List[Grid]={
        getPlayersGrid(playersShipsList, 0)
    }

    @tailrec
    def getPlayersGrid(playersShipsList : List[List[Ship]], cpt : Int, gridList:List[Grid]=List()):List[Grid]={
        
        if(cpt >= playersShipsList.length)gridList
        else getPlayersGrid(playersShipsList, cpt+1,( gridList:+ (new Grid(10,10).insertShipList(playersShipsList(cpt)))))
    }

    def getPlayersActionGrid(playerList : List[Player]):List[ActionList]={
        getPlayersActionGrid(playerList, List())
    }

    @tailrec
    def getPlayersActionGrid(playerList : List[Player], actionList : List[ActionList]):List[ActionList]={
        if(playerList.length == actionList.length) actionList
        else getPlayersActionGrid(playerList, actionList:+new ActionList)
    }
}

