package battleship
import org.scalatest.FunSuite


class ShipSpec extends FunSuite{

    test("constructor basic information test"){
        val ship = new Ship(List.fill(1)(false),'s',1,1)
        assert( ship.length == 1)
        assert(ship.direction== 's')
        assert( ship.coordinateX == 1)
        assert(ship.coordinateY== 1)
    }

    test("constructor orientation = 'north' test"){
        val ship = new Ship(List.fill(2)(false),'n',3,3)
        assert(ship.direction== 's')
        assert(ship.coordinateX == 3)
        assert(ship.coordinateY== 2)
    }
    
    test("constructor orientation = 'west' test"){
        val ship = new Ship(List.fill(2)(false),'w',3,3)
        assert(ship.direction== 'e')
        assert(ship.coordinateX == 2)
        assert(ship.coordinateY== 3)
    }

    test("isInCoordinate(int,int) ship horizontal test"){
        val ship = new Ship(List.fill(3)(false),'e',3,3)
        assert(ship.isInCoordinate(3,3))
        assert(ship.isInCoordinate(4,3))
        assert(ship.isInCoordinate(5,3))
        assert(!(ship.isInCoordinate(2,3)))
        assert(!(ship.isInCoordinate(6,3)))
        assert(!(ship.isInCoordinate(3,2)))
        assert(!(ship.isInCoordinate(3,4)))
    }

    test("isInCoordinate(int,int) ship vertical test"){
        val ship = new Ship(List.fill(3)(false),'s',3,3)
        assert(ship.isInCoordinate(3,3))
        assert(ship.isInCoordinate(3,4))
        assert(ship.isInCoordinate(3,5))
        assert(!(ship.isInCoordinate(3,2)))
        assert(!(ship.isInCoordinate(3,6)))
        assert(!(ship.isInCoordinate(2,3)))
        assert(!(ship.isInCoordinate(4,3)))
    }

    test("isSunk() test"){
        val ship = new Ship(List.fill(1)(false),'s',1,1)
        assert(!(ship.isSunk))
        assert(ship.shoot(1,1).isSunk)
    }
    test("shoot() test"){
        val ship = new Ship(List.fill(2)(false),'s',1,1)
        assert(!(ship.isSunk))
        assert(!ship.shoot(1,1).isSunk)
        assert(ship.shoot(1,1).shoot(1,2).isSunk)
        assert(!ship.shoot(1,1).shoot(1,3).isSunk)
        assert(!ship.shoot(1,1).shoot(1,1).isSunk)
    }
    
}