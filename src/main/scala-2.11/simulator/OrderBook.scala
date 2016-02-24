package simulator

trait OrderBook {
  def cancel(orderId: Int): Unit
  def buyList: List[Order]
  def sellList: List[Order]
  def placeOrder(order: Order): Unit
}
