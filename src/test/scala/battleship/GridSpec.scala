package battleship
import org.scalatest.FunSuite


class GridSpec extends FunSuite{

    test("constructor basic information test"){
        val grid = new Grid(10,10)
        assert(grid.length == 10)
        assert(grid.heigth == 10)
        assert(grid(9,9)=="w")
    }

    test("changeGridTile(Int,Int,String) test"){
        val grid = new Grid(10,10)
        assert(grid(9,9)=="w")
        val newGrid= grid.changeGridTile(9,9,"z")
        assert(newGrid(9,9)=="z")
    }

    test("insertShip(Ship) test"){
        val grid = new Grid(10,10).insertShip(new Ship(List.fill(3)(false),'s',3,3)).insertShip(new Ship(List.fill(3)(false),'e',1,1))
        assert(grid(2,2)=="X")
        assert(grid(3,2)=="X")
        assert(grid(4,2)=="X")

        assert(grid(0,0)=="X")
        assert(grid(0,1)=="X")
        assert(grid(0,2)=="X")
        
    }

    test("shoot(Int,Int) test"){
        val grid = new Grid(10,10).shoot(2,1)
        assert(grid(2,1)=="w_hit")
        assert(grid.insertShip(new Ship(List.fill(3)(false),'e',1,1)).shoot(0,0)(0,0) == "X_hit")
    }
}