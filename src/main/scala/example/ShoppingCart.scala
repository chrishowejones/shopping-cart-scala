package example

object ShoppingCart {

  def checkout(items: List[String]): Double = {
    items.filter(_ != null)
      .foldLeft(0.0)((total, item) =>
        item.toLowerCase match {
          case "apple" => total + 0.60
          case "orange" => total + 0.25
          // case "plum" => total + 0.75
          case _ => throw new IllegalStateException("Illegal item added")
        })
  }
}
