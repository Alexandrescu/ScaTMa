package simulator

import org.scalatest.FlatSpec

class OrderBookTest extends FlatSpec {
  "An order book" should "accept Limit Sell orders" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Sell, 20, 50)

    orderBook.placeOrder(limitOrder)

    assert(orderBook.sellList.size == 1)
    assert(orderBook.sellList.head == limitOrder)
  }
  it should "accept Limit Buy orders" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Buy, 20, 50)

    orderBook.placeOrder(limitOrder)

    assert(orderBook.buyList.size == 1)
    assert(orderBook.buyList.head == limitOrder)
  }
  it should "process Market Buy orders when Limit Sell orders exist" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Sell, 20, 50)

    orderBook.placeOrder(limitOrder)
    assert(orderBook.sellList.size == 1)

    val marketOrder = MarketOrder(2, 2, Buy, 10)
    orderBook.placeOrder(marketOrder)
    assert(orderBook.sellList.size == 1)

    val marketOrder2 = MarketOrder(3, 2, Buy, 10)
    orderBook.placeOrder(marketOrder2)
    assert(orderBook.sellList.isEmpty)
  }
  it should "process Market Sell orders when Limit Buy orders exist" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Buy, 20, 50)

    orderBook.placeOrder(limitOrder)
    assert(orderBook.buyList.size == 1)

    val marketOrder = MarketOrder(2, 2, Sell, 10)
    orderBook.placeOrder(marketOrder)
    assert(orderBook.buyList.size == 1)

    val marketOrder2 = MarketOrder(3, 2, Sell, 10)
    orderBook.placeOrder(marketOrder2)
    assert(orderBook.buyList.isEmpty)
  }
  it should "clear Market orders when there are not sufficient orders in the market" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Buy, 20, 50)
    val marketOrder = MarketOrder(2, 2, Sell, 30)

    orderBook.placeOrder(limitOrder)
    assert(orderBook.buyList.size == 1)

    orderBook.placeOrder(marketOrder)
    assert(orderBook.buyList.isEmpty)
  }
  it should "be able to cancel orders" in {
    val orderBook: OrderBook = new LLOrderBook
    val limitOrder = LimitOrder(1, 1, Sell, 10, 10)

    orderBook.placeOrder(limitOrder)
    assert(orderBook.sellList.size == 1)

    orderBook.cancel(1)
    assert(orderBook.sellList.isEmpty)
  }
}
