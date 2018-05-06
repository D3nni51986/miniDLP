package models

import route.ImperativeRequestContext

abstract class ServiceRequest(id:String)

case class ScanServiceInput(id:String, input:String) extends ServiceRequest(id)
case class ScanFileRequest(id:String, filePath:String) extends ServiceRequest(id)

case class ScanServiceInputContext(input:ScanServiceInput, ctx: ImperativeRequestContext)
case class ScanServiceResult(id:String, resultList:List[String])


case class HandleScanRequest(ctx: ImperativeRequestContext)
case class HandleScanFileRequest(ctx: ImperativeRequestContext)

