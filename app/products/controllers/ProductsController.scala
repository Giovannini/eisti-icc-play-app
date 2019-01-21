package products.controllers

import javax.inject.Inject
import play.api._
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

import products.models.Product
import products.services.ProductsService

class ProductsController @Inject()(
  cc: ControllerComponents,
  productsService: ProductsService
)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def list = Action.async { implicit request =>
    productsService.list.map { products =>
      Ok(Json.toJson(products))
    }
  }

  def lookup(id: Int) = Action.async { implicit request =>
    productsService.lookup(id).map {
      case Some(product) => Ok(Json.toJson(product))
      case None => NotFound(s"No product found with id '$id'")
    }
  }

  def create = Action.async(parse.json) { implicit request =>
    parseJson(request.body).map { case (label, price) =>
      productsService.create(label, price).map { createdId =>
        Created(Json.obj("id" -> createdId))
      }
    }.getOrElse {
      Future.successful(
        BadRequest("Body should contain name: String and price: Int.")
      )
    }.recover {
      case NonFatal(_) => InternalServerError
    }
  }

  private def parsePriceJson(json: JsValue): Option[Int] =
    (json \ "price").validate[Int].asOpt

  private def parseLabelJson(json: JsValue): Option[String] =
    (json \ "label").validate[String].asOpt

  private def parseJson(json: JsValue): Option[(String, Int)] = for {
    label <- parseLabelJson(json)
    price <- parsePriceJson(json)
  } yield (label, price)

  def changeName(id: Int) = Action.async(parse.json) { request =>
    parseLabelJson(request.body) match {
      case None =>
        Future.successful(BadRequest("Body should contain label: String"))
      case Some(newName) =>
        productsService.changeName(id, newName).map {
          case None => NotFound
          case Some(product) => Ok(Json.toJson(product))
        }.recover {
          case NonFatal(_) =>
            InternalServerError("J'ai eu un problème e DB.")
        }
    }
  }

  def changePrice(id: Int) = Action.async(parse.json) { request =>
    parsePriceJson(request.body) match {
      case None =>
        Future.successful(BadRequest("Body should contain price: Int"))
      case Some(newPrice) =>
        productsService.changePrice(id, newPrice).map {
          case None => NotFound
          case Some(product) => Ok(Json.toJson(product))
        }.recover {
          case NonFatal(_) =>
            InternalServerError("J'ai eu un problème e DB.")
        }
    }
  }

  def delete(id: Int) = Action.async {
    productsService.delete(id).map {
      case Some(_) => Ok
      case None => NotFound
    }.recover {
      case NonFatal(_) =>
        InternalServerError("J'ai eu un problème e DB.")
    }
  }
}
