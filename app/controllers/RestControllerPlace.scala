package controllers

import play.api.mvc._
import play.api.libs.json._  // JSON library
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import javax.inject._
import model._

class RestControllerPlace @Inject()(cc:ControllerComponents) extends AbstractController(cc)  {

    // for write objects en json format
    implicit val locationWrites: Writes[Location] = (
        (__ \ "lat").write[Double] and
        (JsPath \ "long").write[Double]
    )(unlift(Location.unapply))

    implicit val placeWrites: Writes[Place] = (
        (JsPath \ "name").write[String] and
        (JsPath \ "location").write[Location]
    )(unlift(Place.unapply))

    // for serialize object json
    implicit val locationReads: Reads[Location] = (
      (JsPath \ "lat").read[Double] (min(-90.0) keepAnd max(90.0)) and
        (JsPath \ "long").read[Double] (min(-180.0) keepAnd max(180.0))
      )(Location.apply _)

    implicit val placeReads: Reads[Place] = (
      (JsPath \ "name").read[String] (minLength[String](2)) and
        (JsPath \ "location").read[Location]
      )(Place.apply _)

    // get places
    def listPlaces = Action {
        val json = Json.toJson(Place.list)
        Ok(json)
    }

  // save places
  def savePlace = Action(parse.json) { request =>
    val placeResult = request.body.validate[Place]
    placeResult.fold(
      errors => {
        BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(errors)))
      },
      place => {
        Place.save(place)
        Ok(Json.obj("status" ->"OK", "message" -> ("Place '"+place.name+"' saved.") ))
      }
    )
  }

}