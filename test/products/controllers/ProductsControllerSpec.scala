package products.controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.any
import play.api.libs.json.{JsValue, Json}
import play.api.test._
import play.api.test.Helpers._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

import products.services.ProductsService
import products.models.Product

class ProductsControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting with MockitoSugar {

  implicit lazy val materializer = app.materializer

  "list" should {
    Seq(
      Seq.empty[Product],
      Seq(Product(1, "Test", 17)),
      Seq(
        Product(1, "Test", 17),
        Product(2, "Foo", 5),
        Product(4, "Bar", 9000),
      )
    ).foreach { products =>
      s"succeed if list returned by service has ${products.length} element(s)" in {
        // Given
        val products: Seq[Product] = Nil
        val productsService = mock[ProductsService]
        val controller = new ProductsController(stubControllerComponents(), productsService)

        // When
        when(productsService.list).thenReturn(Future.successful(products))
        val result = controller.list.apply(FakeRequest(GET, "/"))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(products)
      }
    }
  }

  "lookup" should {
    "return OK with the product if it is found" in {
      // Given
      val productId = 1
      val product = Product(productId, "Test", 17)
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)

      // When
      when(productsService.lookup(productId))
        .thenReturn(Future.successful(Some(product)))
      val result = controller.lookup(productId).apply(FakeRequest())

      // Then
      contentAsJson(result) mustBe Json.toJson(product)
      status(result) mustBe OK
    }

    "return NOT_FOUND if it is not found" in {
      // Given
      val productId = 1
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)

      // When
      when(productsService.lookup(productId))
        .thenReturn(Future.successful(None))
      val result = controller.lookup(productId).apply(FakeRequest())

      // Then
      status(result) mustBe NOT_FOUND
    }
  }

  "create" should {
    "return a CREATED with product id if request is a success" in {
      // Given
      val controller = inject[ProductsController]
      val request = FakeRequest(POST, "/product")
        .withBody(Json.obj(
          "label" -> "test",
          "price" -> 17
        ))

      // When
      val result = controller.create.apply(request)

      // Then
      contentAsJson(result) mustBe Json.obj("id" -> 1)
      status(result) mustBe CREATED
    }

    "return a BAD_REQUEST if request does not contain a label" in {
      // Given
      val controller = inject[ProductsController]
      val request = FakeRequest(POST, "/product")
        .withBody(Json.obj("price" -> 17))

      // When
      val result = controller.create.apply(request)

      // Then
      status(result) mustBe BAD_REQUEST
    }

    "return a BAD_REQUEST if request does not contain a price" in {
      // Given
      val controller = inject[ProductsController]
      val request = FakeRequest(POST, "/product")
        .withBody(Json.obj("label" -> "test"))

      // When
      val result = controller.create.apply(request)

      // Then
      status(result) mustBe BAD_REQUEST
    }

    "return a INTERNAL_SERVER_ERROR if an error occured in service" in {
      // Given
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/product")
        .withBody(Json.obj(
          "label" -> "test",
          "price" -> 17
        ))

      // When
      when(productsService.create(any[String], any[Int]))
        .thenReturn(Future.failed(new Exception("This is a test")))
      val result = controller.create.apply(request)

      // Then
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }

  "changeName" should {
    "return a OK with product id if request is a success" in {
      // Given
      val productId = 17
      val newName = "newTest"
      val updatedProduct = Product(productId, newName, 50)
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("label" -> newName))

      // When
      when(productsService.changeName(productId, newName))
        .thenReturn(Future.successful(Some(updatedProduct)))
      val result = controller.changeName(productId).apply(request)

      // Then
      contentAsJson(result) mustBe Json.toJson(updatedProduct)
      status(result) mustBe OK
    }

    "return a BAD_REQUEST if request does not contain a label" in {
      // Given
      val productId = 17
      val controller = inject[ProductsController]
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("price" -> 17))

      // When
      val result = controller.changeName(productId).apply(request)

      // Then
      status(result) mustBe BAD_REQUEST
    }

    "return a NOT_FOUND if id is not matched" in {
      // Given
      val productId = 17
      val newName = "newTest"
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("label" -> newName))

      // When
      when(productsService.changeName(productId, newName))
        .thenReturn(Future.successful(None))
      val result = controller.changeName(productId).apply(request)

      // Then
      status(result) mustBe NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR if an error occured in service" in {
      // Given
      val productId = 17
      val newName = "newTest"
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("label" -> newName))

      // When
      when(productsService.changeName(productId, newName))
        .thenReturn(Future.failed(new Exception("This is a test")))
      val result = controller.changeName(productId).apply(request)

      // Then
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }

  "changePrice" should {
    "return a OK with product id if request is a success" in {
      // Given
      val productId = 17
      val newPrice = 1000
      val updatedProduct = Product(productId, "test", newPrice)
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("price" -> newPrice))

      // When
      when(productsService.changePrice(productId, newPrice))
        .thenReturn(Future.successful(Some(updatedProduct)))
      val result = controller.changePrice(productId).apply(request)

      // Then
      contentAsJson(result) mustBe Json.toJson(updatedProduct)
      status(result) mustBe OK
    }

    "return a BAD_REQUEST if request does not contain a label" in {
      // Given
      val productId = 17
      val controller = inject[ProductsController]
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("label" -> "test"))

      // When
      val result = controller.changePrice(productId).apply(request)

      // Then
      status(result) mustBe BAD_REQUEST
    }

    "return a NOT_FOUND if id is not matched" in {
      // Given
      val productId = 17
      val newPrice = 1000
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("price" -> newPrice))

      // When
      when(productsService.changePrice(productId, newPrice))
        .thenReturn(Future.successful(None))
      val result = controller.changePrice(productId).apply(request)

      // Then
      status(result) mustBe NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR if an error occured in service" in {
      // Given
      val productId = 17
      val newPrice = 1000
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(POST, "/")
        .withBody(Json.obj("price" -> newPrice))

      // When
      when(productsService.changePrice(productId, newPrice))
        .thenReturn(Future.failed(new Exception("This is a test")))
      val result = controller.changePrice(productId).apply(request)

      // Then
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }

  "delete" should {
    "return a OK with product id if product is deleted" in {
      // Given
      val productId = 17
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(DELETE, "/")

      // When
      when(productsService.delete(productId))
        .thenReturn(Future.successful(Some(Product(productId, "Test", 5))))
      val result = controller.delete(productId).apply(request)

      // Then
      status(result) mustBe OK
    }

    "return a NOT_FOUND if id is not matched" in {
      // Given
      val productId = 17
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(DELETE, "/")

      // When
      when(productsService.delete(productId))
        .thenReturn(Future.successful(None))
      val result = controller.delete(productId).apply(request)

      // Then
      status(result) mustBe NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR if an error occured in service" in {
      // Given
      val productId = 17
      val productsService = mock[ProductsService]
      val controller = new ProductsController(stubControllerComponents(), productsService)
      val request = FakeRequest(DELETE, "/")

      // When
      when(productsService.delete(productId))
        .thenReturn(Future.failed(new Exception("This is a test")))
      val result = controller.delete(productId).apply(request)

      // Then
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }
}
