package simulator

class LLOrderBook extends OrderBook{

  private class Node(var order: LimitOrder, var next: Node)

  private class LinkedList {
    private val list = new Node(null, null)

    /**
      * @param order The order which is going to be added to the list.
      *              The list is kept sorted in order to help order matching.
      * @return A list of actions to take by the Wealth manager.
      */
    def addOrder(order: LimitOrder): List[Action] = {
      val sign = order.oType match {
        case Sell => 1
        case Buy => -1
      }

      /* sign * order.price > sign * node.next.order.price
         sign * (a - b) > 0
       */
      var node = list
      while(node.next != null && sign * (order.price - node.next.order.price) > 0) {
        node = node.next
      }

      val newNode = new Node(order, node.next)
      node.next = newNode

      List()
    }

    def executeOrder(order: MarketOrder): List[Action] = {
      val node = list
      while(order.quantity > 0 && node.next != null) {
        val executing = Math.min(order.quantity, node.next.order.quantity)

        node.next.order.quantity -= executing
        order.quantity -= executing

        /** TODO: Add 2 actions for each party */
        if(node.next.order.quantity == 0) {
          node.next = node.next.next
        }
      }

      List()
    }

    def matchOrder(order: LimitOrder): List[Action] = {
      val sign = order.oType match {
        case Sell => 1
        case Buy => -1
      }

      val node = list
      while(node.next != null && (order.price - node.next.order.price) * sign <= 0) {
        val executing = Math.min(order.quantity, node.next.order.quantity)

        node.next.order.quantity -= executing
        order.quantity -= executing

        /** TODO: Add 2 actions for each party */
        if(node.next.order.quantity == 0) {
          node.next = node.next.next
        }
      }

      List()
    }

    def toList: List[LimitOrder] = {
      var result: List[LimitOrder] = List()
      var node = list
      while(node.next != null) {
        result ::= node.next.order
        node = node.next
      }
      result
    }

    def removeOrder(orderId: Int): Unit = {
      var node = list
      while(node.next != null) {
        if (node.next.order.orderId == orderId) {
          node.next = node.next.next
          return
        }
        node = node.next
      }
    }
  }

  private val sellQueue: LinkedList = new LinkedList
  private val buyQueue: LinkedList = new LinkedList

  override def cancel(orderId: Int): Unit = {
    sellQueue.removeOrder(orderId)
    buyQueue.removeOrder(orderId)
  }

  override def buyList: List[Order] = buyQueue.toList

  override def sellList: List[Order] = sellQueue.toList

  override def placeOrder(order: Order): Unit = {
    order match {
      case marketOrder @ MarketOrder(_, _, Buy, _) =>
        sellQueue.executeOrder(marketOrder)
      case marketOrder @ MarketOrder(_, _, Sell, _) =>
        buyQueue.executeOrder(marketOrder)
      case limitOrder @ LimitOrder(_, _, Buy, _, _) =>
        sellQueue.matchOrder(limitOrder)
        buyQueue.addOrder(limitOrder)
      case limitOrder @ LimitOrder(_, _, Sell, _, _) =>
        buyQueue.matchOrder(limitOrder)
        sellQueue.addOrder(limitOrder)
    }
  }
}
