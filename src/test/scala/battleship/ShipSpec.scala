package battleship
import org.scalatest.FunSuite


class ShipSpec extends FunSuite{

    test("constructor basic information test"){
        val ship = new Ship(List.fill(1)(false),'s',1,1)
        assert( ship.length == 1)
        assert(ship.direction== 's')
        assert( ship.posX == 1)
        assert(ship.posY== 1)
    }

    test("constructor orientation = 'north' test"){
        val ship = new Ship(List.fill(2)(false),'n',3,3)
        assert(ship.direction== 's')
        assert(ship.posX == 3)
        assert(ship.posY== 2)
    }
    
    test("constructor orientation = 'west' test"){
        val ship = new Ship(List.fill(2)(false),'w',3,3)
        assert(ship.direction== 'e')
        assert(ship.posX == 2)
        assert(ship.posY== 3)
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
    test("shoot(int,int) test"){
        val ship = new Ship(List.fill(2)(false),'s',1,1)
        assert(!(ship.isSunk))
        assert(!ship.shoot(1,1).isSunk)
        assert(ship.shoot(1,1).shoot(1,2).isSunk)
        assert(!ship.shoot(1,1).shoot(1,3).isSunk)
        assert(!ship.shoot(1,1).shoot(1,1).isSunk)
    }

    test("isShootable(int,int) test"){
        val ship = new Ship(List.fill(1)(false),'s',1,1)
        assert(ship.isShootable(1,1))
        assert(!(ship.shoot(1,1).isShootable(1,1)))
    }

    test("collision(ship) test"){
        val ship1 = new Ship(List.fill(3)(false),'s',2,1)
        val ship2 = new Ship(List.fill(3)(false),'e',1,2)
        
        val ship3 = new Ship(List.fill(1)(false),'s',1,1)
        val ship4 = new Ship(List.fill(1)(false),'s',2,2)
        assert(ship1.collision(ship1))
        assert(ship1.collision(ship2))
        assert(!(ship1.collision(ship3)))
        assert((ship1.collision(ship4)))
        assert(!(ship2.collision(ship3)))
        assert((ship2.collision(ship4)))
        assert(!(ship3.collision(ship4)))
        assert(ship1.collision(new Ship(List.fill(3)(false),'s',2,3)))
        assert(ship1.collision(new Ship(List.fill(1)(false),'s',2,2)))
    }
}