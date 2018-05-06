package support

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import models.{ScanFileRequest, ScanServiceInput, ScanServiceResult}
import spray.json.DefaultJsonProtocol

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val scanServiceInputFormat = jsonFormat2(ScanServiceInput)
  implicit val scanServiceResultFormat = jsonFormat2(ScanServiceResult)
  implicit val scanFileFormat = jsonFormat2(ScanFileRequest)
}

