package scalaDHT

import java.net.InetAddress
import org.apache.commons.lang3.StringUtils

class Contact(n: Int, a: InetAddress, p: Int, o: Boolean) {
  val nodeID: Int = n
  val address: InetAddress = a
  val port: Int = p
  val isOwner: Boolean = o
  def getBitRep: List[Boolean] = {
    StringUtils.leftPad(nodeID.toBinaryString, 32, '0').toList.map(c => if (c == '1') true else false)
  }
}

trait RoutingTable {
  val bucketSize: Int
  val alpha: Int
  def add(contact: Contact): Boolean
  def getClosestAlpha(nodeID: Int): List[Contact]
}

/**
 * Binary-tree implementation of the kademlia routing table
 * Not thread-safe
 * constructor expects function that returns boolean for whether or not to replace
 */
class BinaryTreeRoutingTable(owner: Contact, k: Int, d: Int, a: Int, fn: Contact => Boolean) extends RoutingTable {
  /**
   * inner node class with value (0 or 1), right and left, and payload (list of k),
   * and replacement cache (unbounded list)
   */
  class Node(b: List[Contact], r: List[Contact]) {
    // should maybe switch to using Either, since either leaf <==> having buckets
    var bucket: Option[List[Contact]] = Some(b)
    var replacementCache: Option[List[Contact]] = Some(r)
    var left: Option[Node] = None
    var right: Option[Node] = None
  }

  override val bucketSize: Int = k
  override val alpha: Int = a
  val alwaysSplitDepth: Int = d
  val replaceContactFn = fn
  val root: Node = new Node(List[Contact](owner), List[Contact]())

  /**
   *
   * @param nodeID
   * @return
   */
  override def getClosestAlpha(nodeID: Int): List[Contact] = {
    val bitRep = nodeID.toBinaryString.toList.map(c => if (c == '1') true else false)
    recursiveGet(root, bitRep, alpha).take(alpha)
  }

  def recursiveGet(currNode: Node, bits: List[Boolean], leftToGet: Int): List[Contact] = {
    assert((currNode.left.isDefined && currNode.right.isDefined)
      ||(currNode.left.isEmpty && currNode.left.isEmpty))

    if (bits.head && currNode.left.isDefined) {
      // go left
      var contacts = recursiveGet(currNode.left.get, bits.tail, leftToGet)
      var missing = leftToGet - contacts.length
      if (missing > 0 && currNode.bucket.isEmpty)
        contacts = contacts ::: recursiveGet(currNode.right.get, bits, missing)
      return contacts
    } else if (!bits.head && currNode.right.isDefined) {
      var contacts = recursiveGet(currNode.right.get, bits.tail, leftToGet)
      val missing = leftToGet - contacts.length
      if (missing > 0 && currNode.bucket.isEmpty)
        contacts = contacts ::: recursiveGet(currNode.left.get, bits, missing)
      return contacts
    } else
      return currNode.bucket.get
  }

  override def add(contact: Contact): Boolean = {
    val bits = contact.getBitRep
    recursiveAdd(root, contact, bits, 0)
  }

  /**
   * Following bits, going right or left until you can descend no further; then try to insert into kbucket
   * @param currNode
   * @param contact
   * @param bits
   * @param depth
   * @return
   */
  private def recursiveAdd(currNode: Node, contact: Contact, bits: List[Boolean], depth: Int): Boolean = {
    // TODO: what if you attempt to split deeper than the number of bits?
    if (bits.head && currNode.left.isDefined) {
      recursiveAdd(currNode.left.get, contact, bits.tail, depth + 1)
    } else if (!bits.head && currNode.right.isDefined) {
      recursiveAdd(currNode.right.get, contact, bits.tail, depth + 1)
    } else {
      insertIntoLeafBucket(currNode, contact, bits, depth)
    }
  }

  /**
   *
   * @param leaf
   * @param contact
   * @return
   */
  private def insertIntoLeafBucket(leaf: Node, contact: Contact, bits: List[Boolean], depth: Int): Boolean = {
    if (leaf.bucket.get.length < bucketSize) {
      leaf.bucket = Some(contact :: leaf.bucket.get)
      return true
    } else if (depth <= alwaysSplitDepth || leaf.bucket.get.find(x => x.isOwner).isDefined) {

      val leftBucket = leaf.bucket.get.filter(x => x.getBitRep.drop(depth).head)
      val rightBucket = leaf.bucket.get.filter(x => !x.getBitRep.drop(depth).head)
      val leftReplacementCache = leaf.replacementCache.get.filter(x => x.getBitRep.drop(depth).head)
      val rightReplacementCache = leaf.replacementCache.get.filter(x => !x.getBitRep.drop(depth).head)
      leaf.bucket = None
      leaf.replacementCache = None
      leaf.left = Some(new Node(leftBucket, leftReplacementCache))
      leaf.right = Some(new Node(rightBucket, rightReplacementCache))
      if (bits.head)
        insertIntoLeafBucket(leaf.left.get, contact, bits.tail, depth + 1)
      else
        insertIntoLeafBucket(leaf.right.get, contact, bits.tail, depth + 1)
    } else {
      leaf.replacementCache = Some(contact::leaf.replacementCache.get)
      return false
    }
  }
}

