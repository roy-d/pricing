package pricing

sealed trait Item

case class SingleItem(id: String) extends Item

case class BundledItem(id: String, items: Map[Item, Int]) extends Item

case class Cart(items: Map[Item, Int])

case class Catalog(items: Map[Item, Double])

class Pricing(catalog: Catalog, bundles: List[BundledItem]) {

  def getMinPrice(cart: Cart): Double = generateCarts(cart).map(price).min

  def price(cart: Cart) =
    cart.items.map { case (item, count) ⇒ catalog.items(item) * count }.foldLeft(0.0)(_ + _)

  def generateCarts(cart: Cart): List[Cart] = {
    def helper(possibleCarts: List[Cart]): List[Cart] = {
      val newCarts: List[Cart] =
        for {
          possibleCart ← possibleCarts
          bundle ← bundles
          if canBundleBeApplied(possibleCart, bundle)
        } yield {
          applyBundleToCart(possibleCart, bundle)
        }
      newCarts match {
        case Nil ⇒ possibleCarts
        case _ ⇒ possibleCarts ::: helper(newCarts)
      }
    }
    helper(List(cart))
  }

  def canBundleBeApplied(cart: Cart, bundle: BundledItem): Boolean =
    !bundle.items.map { case (item, quantity) ⇒ cart.items.contains(item) && cart.items(item) >= quantity }.exists(!_)

  def applyBundleToCart(cart: Cart, bundle: BundledItem): Cart = {
    val newItems = cart.items.map { case (item, quantity) ⇒
      if (bundle.items.contains(item) && bundle.items(item) <= quantity) (item, quantity - bundle.items(item)) else (item, quantity)
    }.filter { case (_, quantity) ⇒ quantity > 0 }

    if (newItems.contains(bundle)) Cart(newItems.+(bundle → (newItems(bundle) + 1))) else Cart(newItems.+(bundle → 1))
  }

}
