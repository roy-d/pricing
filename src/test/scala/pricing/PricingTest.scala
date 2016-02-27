package pricing

import org.scalatest.{FlatSpec, MustMatchers}

class PricingTest extends FlatSpec with MustMatchers {

  "Pricing" should "can do simple pricing" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 1,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.price(cart) mustBe 1.0 + 2.0 + 3.0
  }

  "Pricing" should "check if bundles can be applied to cart" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 1,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.canBundleBeApplied(cart, BundledItem("11", Map(SingleItem("1") → 2))) mustBe false

    pricing.canBundleBeApplied(cart, BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))) mustBe true
  }

  "Pricing" should "check if bundles can be applied to cart-multiple" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 2,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.canBundleBeApplied(cart, BundledItem("11", Map(SingleItem("1") → 2))) mustBe true

    pricing.canBundleBeApplied(cart, BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))) mustBe true
  }

  "Pricing" should "apply bundle to cart if possible" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 1,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.applyBundleToCart(cart, BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))) mustBe Cart(Map(
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1,
      SingleItem("3") → 1
    ))
  }

  "Pricing" should "apply bundle to cart if possible-multiple" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 2,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.applyBundleToCart(cart, BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))) mustBe Cart(Map(
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1,
      SingleItem("3") → 1,
      SingleItem("1") → 1
    ))

    pricing.applyBundleToCart(cart, BundledItem("11", Map(SingleItem("1") → 2))) mustBe Cart(Map(
      BundledItem("11", Map(SingleItem("1") → 2)) → 1,
      SingleItem("3") → 1,
      SingleItem("2") → 1
    ))
  }

  "Pricing" should "generates carts" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 1,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.generateCarts(cart).size mustBe 2
  }

  "Pricing" should "generates carts - multiple" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 2,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.generateCarts(cart).size mustBe 3
  }

  "Pricing" should "return the minimum possible price" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 1,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.getMinPrice(cart) mustBe 1.5 + 3.0
  }

  "Pricing" should "return the minimum possible price - multiple" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 2,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.getMinPrice(cart) mustBe 1.0 + 1.5 + 3.0
  }


  "Pricing" should "return the minimum possible price - multiple 1" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 1.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 3,
      SingleItem("2") → 1,
      SingleItem("3") → 1
    ))

    pricing.getMinPrice(cart) mustBe 0.75 + 1.5 + 3.0
  }

  "Pricing" should "return the minimum possible price - multiple 2" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 0.75,
      BundledItem("122", Map(SingleItem("1") → 1, SingleItem("2") → 2)) → 1.5,
      BundledItem("34", Map(SingleItem("3") → 1, SingleItem("4") → 1)) → 6.0
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("122", Map(SingleItem("1") → 1, SingleItem("2") → 2)),
      BundledItem("34", Map(SingleItem("3") → 1, SingleItem("4") → 1))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 3,
      SingleItem("2") → 3,
      SingleItem("3") → 1,
      SingleItem("4") → 1
    ))

    pricing.generateCarts(cart).size mustBe 16
    pricing.getMinPrice(cart) mustBe 0.75 + 1.5 + 2.0 + 6.0
  }

  "Pricing" should "return the minimum possible price - multiple 3" in {
    val catalog = Catalog(Map(
      SingleItem("1") → 1.0,
      SingleItem("2") → 2.0,
      SingleItem("3") → 3.0,
      SingleItem("4") → 4.0,
      BundledItem("11", Map(SingleItem("1") → 2)) → 1.5,
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)) → 2.5,
      BundledItem("22", Map(SingleItem("2") → 2)) → 3.5
    ))
    val bundles = List(
      BundledItem("11", Map(SingleItem("1") → 2)),
      BundledItem("12", Map(SingleItem("1") → 1, SingleItem("2") → 1)),
      BundledItem("22", Map(SingleItem("2") → 2))
    )
    val pricing = new Pricing(catalog, bundles)

    val cart = Cart(Map(
      SingleItem("1") → 2,
      SingleItem("2") → 2
    ))

    pricing.generateCarts(cart).
      map(c ⇒ (c, pricing.price(c))).
      foreach { case (c, price) ⇒ println(c); println(price) }

    pricing.generateCarts(cart).size mustBe 7
    pricing.getMinPrice(cart) mustBe 5.0
  }
}
