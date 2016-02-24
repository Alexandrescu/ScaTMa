package simulator

abstract class Order(val oType: OrderType)
case class MarketOrder(orderId: Int, traderId: Int, override val oType: OrderType, var quantity: Int) extends Order(oType)
case class LimitOrder(orderId: Int, traderId: Int, override val oType: OrderType, var quantity: Int, val price: Double) extends Order(oType)

abstract class OrderType
case object Sell extends OrderType
case object Buy extends OrderType