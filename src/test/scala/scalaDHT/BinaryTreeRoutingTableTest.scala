package scalaDHT

import java.net.InetAddress

import org.scalatest.FunSuite

/**
 * Created by sctu on 1/4/15.
 */
class BinaryTreeRoutingTableTest extends FunSuite {
  val nodeIDs:List[Int] = List(
    Math.pow(2, 31).toInt, // left
    Math.pow(2, 30).toInt+2, //
    Math.pow(2, 30).toInt+Math.pow(2, 29).toInt,
    Math.pow(2, 30).toInt+Math.pow(2, 29).toInt + 2)
  val owner = new Contact(Math.pow(2, 30).toInt, InetAddress.getLocalHost, Math.pow(2, 30).toInt, true)
  val contacts:List[Contact] = nodeIDs.map(x => new Contact(x, InetAddress.getLocalHost, x, false))

  test("can make bucket of size 5 at root") {
    val rt:BinaryTreeRoutingTable = new BinaryTreeRoutingTable(owner, 5, 2, 2, x => true)
    contacts.foreach(rt.add)

    val closest = rt.getClosestAlpha(owner.nodeID)
    assert(closest.length == 2)
    assert(closest.head.port == (Math.pow(2, 30).toInt+Math.pow(2, 29).toInt + 2))
    assert(closest.tail.head.port == (Math.pow(2, 30).toInt+Math.pow(2, 29).toInt))
  }

  test("can split twice") {
    val rt:BinaryTreeRoutingTable = new BinaryTreeRoutingTable(owner, 2, 2, 3, x => true)
    contacts.foreach(rt.add)
    val closest = rt.getClosestAlpha(owner.nodeID)
    assert(closest.length == 3)
    assert(rt.root.right.isDefined)
    assert(rt.root.right.get.right.isDefined)
    assert(closest.head.port == (Math.pow(2, 30).toInt + 2))
    assert(closest.tail.head.port == owner.port)
    assert(closest.tail.tail.head.port == (Math.pow(2, 30).toInt+Math.pow(2, 29).toInt))
  }
}
