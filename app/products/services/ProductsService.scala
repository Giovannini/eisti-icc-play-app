package products.services

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

import javax.inject.{Inject, Singleton}
import products.models.Product

@Singleton
class ProductsService @Inject()(implicit ec: ExecutionContext) {
  var products: Seq[Product] = Seq()
  var _nextId = 0

  private def nextId(): Int = {
    _nextId = _nextId + 1
    _nextId
  }

  def list: Future[Seq[Product]] = Future {
    products
  }

  def lookup(id: Int): Future[Option[Product]] = Future {
    products.find(_.id == id)
  }

  def create(label: String, price: Int): Future[Int] = Future {
    val generatedId: Int = nextId()
    val newProduct = Product(generatedId, label, price)
    products = products :+ newProduct
    generatedId
  }

  def changeName(id: Int, newName: String): Future[Option[Product]] =
    Future {
      val (updatedProducts, optionProduct) =
        products.foldLeft((Seq.empty[Product], Option.empty[Product])) {
          case ((acc, _), product) if product.id == id =>
            val updatedProduct = product.copy(label = newName)
            (acc :+ updatedProduct, Some(updatedProduct))

          case ((acc, option), product) =>
            (acc :+ product, option)
        }
      products = updatedProducts
      optionProduct
    }

  def changePrice(id: Int, newPrice: Int): Future[Option[Product]] =
    Future {
      val (updatedProducts, optionProduct) =
        products.foldLeft((Seq.empty[Product], Option.empty[Product])) {
          case ((acc, _), product) if product.id == id =>
            val updatedProduct = product.copy(price = newPrice)
            (acc :+ updatedProduct, Some(updatedProduct))

          case ((acc, option), product) =>
            (acc :+ product, option)
        }
      products = updatedProducts
      optionProduct
    }

  def delete(id: Int): Future[Option[Product]] = Future {
    val (productsWithId, newProducts) = products.partition(_.id == id)
    products = newProducts
    productsWithId.headOption
  }
}
