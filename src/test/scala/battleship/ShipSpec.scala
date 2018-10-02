package battleship
import org.scalatest.FunSuite


object ShipSpec extends FunSuite{

    test("pliz"){
        assert(1==1)
    }

    test("test double "){
        assert(new Ship().twice(2) == 2*2)
    }
}