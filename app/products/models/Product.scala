package products.models

import play.api.libs.json.{Format, Json}

final case class Product(
  id: Int,
  label: String,
  price: Int
)

object Product {
  implicit val format: Format[Product] = Json.format[Product]
}
