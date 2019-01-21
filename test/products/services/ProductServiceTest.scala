package products.services

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import org.scalatest.concurrent.ScalaFutures
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.any
import play.api.libs.json.{JsValue, Json}
import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

import products.models.Product


class ProductsServiceSpec extends PlaySpec with ScalaFutures {

  "list" should {
    "return an empty list if service has just been created" in {
      // Given
      val service = new ProductsService()

      // When
      val result = service.list

      // Then
      whenReady(result) { list =>
        assert(list.isEmpty)
      }
    }
  }

  "create" should {
    "add a new product to the product's list" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        list <- service.list
      } yield (productId, list)) { case (id, list) =>
        assert(list == Seq(Product(id, productLabel, productPrice)))
      }
    }

    "add several new products to the product's list" in {
      // Given
      val service = new ProductsService()
      val productLabel1 = "Test"
      val productPrice1 = 17
      val productLabel2 = "Test2"
      val productPrice2 = 18

      // Then
      whenReady(for {
        productId1 <- service.create(productLabel1, productPrice1)
        productId2 <- service.create(productLabel2, productPrice2)
        list <- service.list
      } yield (productId1, productId2, list)) { case (id1, id2, list) =>
        assert(list == Seq(
          Product(id1, productLabel1, productPrice1),
          Product(id2, productLabel2, productPrice2),
        ))
      }
    }
  }

  "lookup" should {
    "not find a product that does not exist in an empty list" in {
      // Given
      val service = new ProductsService()

      // Then
      whenReady(for {
        maybeProduct <- service.lookup(1)
      } yield maybeProduct) { maybeProduct =>
        assert(maybeProduct.isEmpty)
      }
    }

    "not find a product that does not exist in a non empty list" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        maybeProduct <- service.lookup(productId + 5)
      } yield maybeProduct) { maybeProduct =>
        assert(maybeProduct.isEmpty)
      }
    }

    "find a product that does exist in a list" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        maybeProduct <- service.lookup(productId)
      } yield maybeProduct) { maybeProduct =>
        assert(maybeProduct.nonEmpty)
      }
    }
  }

  "changeName" should {
    "update the name of an existing product" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17
      val newLabel = s"New${productLabel}"

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        _ <- service.changeName(productId, newLabel)
        list <- service.list
      } yield (productId, list)) { case (id, list) =>
        assert(list == Seq(Product(id, newLabel, productPrice)))
      }
    }

    "not update if product does not exist" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17
      val newLabel = s"New${productLabel}"

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        _ <- service.changeName(productId + 1, newLabel)
        list <- service.list
      } yield (productId, list)) { case (id, list) =>
        assert(list == Seq(Product(id, productLabel, productPrice)))
      }
    }
  }

  "changePrice" should {
    "update the name of an existing product" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17
      val newPrice = productPrice + 5

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        _ <- service.changePrice(productId, newPrice)
        list <- service.list
      } yield (productId, list)) { case (id, list) =>
        assert(list == Seq(Product(id, productLabel, newPrice)))
      }
    }

    "not update if product does not exist" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17
      val newPrice = productPrice + 5

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        _ <- service.changePrice(productId + 1, newPrice)
        list <- service.list
      } yield (productId, list)) { case (id, list) =>
        assert(list == Seq(Product(id, productLabel, productPrice)))
      }
    }
  }

  "delete" should {
    "return None if no element is deleted" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        deletionResult <- service.delete(productId + 1)
      } yield deletionResult) { case result =>
        assert(result.isEmpty)
      }
    }

    "return Some(_) and update the list if one element is deleted" in {
      // Given
      val service = new ProductsService()
      val productLabel = "Test"
      val productPrice = 17

      // Then
      whenReady(for {
        productId <- service.create(productLabel, productPrice)
        deletionResult <- service.delete(productId)
        list <- service.list
      } yield (deletionResult, list)) { case (result, list) =>
        assert(result.nonEmpty)
        assert(list.isEmpty)
      }
    }
  }

}
