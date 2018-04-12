
import {HttpErrorResponse} from "@angular/common/http";
import {ErrorObservable} from "rxjs/observable/ErrorObservable";
import {ResponseData} from "./models";

export class Exception {

  private static debug = true
  private static ok = 0
  private static businessExceptionHandlers:{} = {}
  private static httpExceptionHandlers:{} = {}

  private static emitHttpException(err:HttpErrorResponse,handler:()=>{}){
    console.error(err)
    if(handler) handler()
    else if(this.debug) alert(`HttpException : ${err.status}|${err.name}|${err.message}|${err.error}`)
  }

  private static emitBusinessException(resp:ResponseData,handler:(resp?:ResponseData)=>{}){
    console.log(resp)
    if(handler) handler(resp)
    else if(this.debug) alert(`BusinessException : ${JSON.stringify(resp)}`)
  }

  static addHttpExceptionHandler(status:number,handler:()=>void){
    this.httpExceptionHandlers[status] = handler
  }

  static addBusinessExceptionHandler(tag:string,code:number,handler:(resp?:ResponseData)=>void){
    if(!this.businessExceptionHandlers[tag]) this.businessExceptionHandlers[tag] = {}
    this.businessExceptionHandlers[tag][code] = handler
  }

  static handleHttpException(err:HttpErrorResponse){
    let handler = null
    for(let status in this.httpExceptionHandlers)
    {
      if(err.status.toString() == status){
        handler = this.httpExceptionHandlers[status]
        break
      }
    }
    Exception.emitHttpException(err,handler)
    return new ErrorObservable('http exception')
  }

  static handleBusinessException(tag:string,resp:ResponseData):boolean {
    if(resp.code != this.ok){
      let handler = null
      for(let key in this.businessExceptionHandlers){
        if(key == tag){
          for(let code in this.businessExceptionHandlers[key]){
            if(code == resp.code.toString()){
              handler = this.businessExceptionHandlers[key][code]
              break
            }
          }
          break
        }
      }
      Exception.emitBusinessException(resp,handler)
      return false
    }else return true
  }

}
