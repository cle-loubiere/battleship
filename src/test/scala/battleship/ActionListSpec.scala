package battleship
import org.scalatest.FunSuite


class ActionListSpec extends FunSuite{

    test("constructor basic information test"){
        val actionList = new ActionList
        val newActionlist = actionList.addAction(1,1,"hit")
        assert(newActionlist.actionList.length == 1)
        assert(newActionlist.hasLastShotHitAndNotSunk == true)
        assert(newActionlist.areCoordinatesAlreadyTargeted(1,1) == true)
        assert(newActionlist.areCoordinatesAlreadyTargeted(0,0) == false)
    }
}