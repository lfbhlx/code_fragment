package utils

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.charset.Charset
import java.security.cert.X509Certificate
import java.util.ArrayList

import org.apache.http.HttpResponse
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{HttpGet, HttpPost}
import org.apache.http.config.RegistryBuilder
import org.apache.http.conn.socket.{ConnectionSocketFactory, PlainConnectionSocketFactory}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import org.apache.http.ssl.{SSLContextBuilder, TrustStrategy}
import org.apache.http.conn.ssl.NoopHostnameVerifier
import org.apache.http.conn.ssl.SSLConnectionSocketFactory
import org.apache.http.entity.StringEntity
import org.apache.http.entity.mime.MultipartEntityBuilder
import org.apache.http.message.BasicNameValuePair
import play.api.libs.json.Json

trait Requestable {
  def getMethod:String
  def request(client:ScaHttpClient):ScaHttpResponse
}

class ScaHttpResponse(val httpResponse:HttpResponse) {
  val statusCode = httpResponse.getStatusLine.getStatusCode
  val headers = httpResponse.getAllHeaders.map{h =>
    (h.getName,h.getValue)
  }.toMap

}
object ScaHttpResponse {

  implicit class ScaHttpResponse2Text(resp:ScaHttpResponse) {
    def asText:String = {
      val br = new BufferedReader(new InputStreamReader(resp.httpResponse.getEntity.getContent))
      val sb = new StringBuilder()
      var line = br.readLine()
      while(line != null)
      {
        sb.append(line)
        line = br.readLine()
      }
      br.close()
      sb.toString()
    }
  }

  implicit class ScaHttpResponse2JsValue(resp:ScaHttpResponse) {
    def asJson = Json.parse(resp.asText)
  }

  def apply(httpResponse: HttpResponse): ScaHttpResponse = new ScaHttpResponse(httpResponse)
}

class ScaHttpGet(
                  val url:String ,
                  val headers:Map[String,String] = Map[String,String]() ,
                  val requestConfig: Option[RequestConfig] = None) extends Requestable{
  val httpGet = new HttpGet(url)
  for((k,v) <- headers){
    httpGet.setHeader(k,v)
  }
  requestConfig match {
    case Some(conf) => httpGet.setConfig(conf)
    case None =>
  }

  def getMethod: String = httpGet.getMethod

  def request(client: ScaHttpClient) = ScaHttpResponse(client.httpClient.execute(httpGet))
}
object ScaHttpGet {
  def apply(url: String,headers: Map[String, String] = Map[String,String](),requestConfig: Option[RequestConfig] = None): ScaHttpGet = {
    new ScaHttpGet(url,headers,requestConfig)
  }
}

class ScaHttpPost(
                   val url:String ,
                   val headers:Map[String,String] = Map[String,String]() ,
                   val requestConfig:Option[RequestConfig] = None) extends Requestable{
  val httpPost = new HttpPost(url)
  for((k,v) <- headers){
    httpPost.setHeader(k,v)
  }
  requestConfig match {
    case Some(conf) => httpPost.setConfig(conf)
    case None =>
  }

  def setBody(body:Any,charset:String="UTF-8") = {
    val setUrlFormDataBody = (bodyMap:Map[String,Any])=>{
      val nvList = new ArrayList[BasicNameValuePair]()
      bodyMap.map{kv=>
        new BasicNameValuePair(kv._1,kv._2.toString)
      }.foreach{item =>
        nvList.add(item)
      }
      httpPost.setEntity(new UrlEncodedFormEntity(nvList,charset))
    }
    body match {
      case bodyMap:Map[String,Any] => setUrlFormDataBody(bodyMap)
      case (false,bodyMap:Map[String,Any]) => setUrlFormDataBody(bodyMap)
      case (true,bodyMap:Map[String,Any]) =>
        val entityBuilder = MultipartEntityBuilder.create().setCharset(Charset.forName(charset))
        bodyMap foreach {kv =>
          kv match {
            case (key:String,value:String) => entityBuilder.addTextBody(key,value)
            case (key:String,value:File) => entityBuilder.addBinaryBody(key,value)
            case (key:String,value:InputStream) => entityBuilder.addBinaryBody(key,value)
            case (key:String,value:Array[Byte]) => entityBuilder.addBinaryBody(key,value)
            case _=>
          }
        }
        httpPost.setEntity(entityBuilder.build())
      case bodyStr:String => httpPost.setEntity(new StringEntity(bodyStr,charset))
      case _=> throw new RuntimeException("Can support this type of body argument")
    }
    this
  }

  def getMethod = httpPost.getMethod

  def request(client: ScaHttpClient) = ScaHttpResponse(client.httpClient.execute(httpPost))
}
object ScaHttpPost {
  def apply(
             url: String,
             headers: Map[String, String] = Map[String, String](),
             requestConfig: Option[RequestConfig] = None): ScaHttpPost = new ScaHttpPost(url,headers,requestConfig)
}

class ScaHttpClient(val httpClient:CloseableHttpClient) {
  var REQUEST_CONFIG = RequestConfig.copy(RequestConfig.DEFAULT).build()
  var DEFAULT_HEADERS = Map[String,String]()

  def get(url:String,headers:Option[Map[String,String]] = None):ScaHttpResponse = {
    ScaHttpGet(url,headers.getOrElse(DEFAULT_HEADERS),Some(REQUEST_CONFIG)).request(this)
  }

  def getWithQuery(baseUrl:String,queryMap:Map[String,String],headers:Option[Map[String,String]] = None):ScaHttpResponse = {
    val queryString = queryMap.map{p=>
      s"${p._1}=${p._2}"
    }.mkString("&")
    get(s"${baseUrl}?${queryString}",headers)
  }

  def post(url:String,body:String,headers:Option[Map[String,String]] = None):ScaHttpResponse = {
    ScaHttpPost(url,headers.getOrElse(DEFAULT_HEADERS),Some(REQUEST_CONFIG)).setBody(body).request(this)
  }

  def postWithMap(url:String,body:Map[String,Any],isMulti:Boolean = false,headers:Option[Map[String,String]] = None):ScaHttpResponse = {
    ScaHttpPost(url,headers.getOrElse(DEFAULT_HEADERS),Some(REQUEST_CONFIG)).setBody((isMulti,body)).request(this)
  }

  def close() = httpClient.close()

}
object ScaHttpClient {
  def apply(httpClient: CloseableHttpClient): ScaHttpClient = new ScaHttpClient(httpClient)
}

object ScaHttp {

  val DEFAULT_SSL_CONNECTION_SOCKET_FACTORY = getDefaultSSLConnectionSocketFactory(new TrustStrategy(){
    def isTrusted(chain: Array[X509Certificate], authType: String) = {
      true
    }
  })
  val DEFAULT_POOLING_HTTP_CLIENT_CONNECTION_MANAGER = getDefaultPoolingHttpClientConnectionManager(500)

  private def getDefaultSSLConnectionSocketFactory(trustStrategy: TrustStrategy) = {
    val builder = new SSLContextBuilder()
    builder.loadTrustMaterial(null,trustStrategy)
    new SSLConnectionSocketFactory(builder.build, Array[String]("SSLv2Hello", "SSLv3", "TLSv1", "TLSv1.2"), null, NoopHostnameVerifier.INSTANCE)
  }

  private def getDefaultPoolingHttpClientConnectionManager(maxTotal:Int) = {
    val registry = RegistryBuilder.create[ConnectionSocketFactory]()
      .register("http",new PlainConnectionSocketFactory())
      .register("https",DEFAULT_SSL_CONNECTION_SOCKET_FACTORY)
      .build()
    val cm = new PoolingHttpClientConnectionManager(registry)
    cm.setMaxTotal(maxTotal)
    cm
  }

  def client:ScaHttpClient = ScaHttpClient(HttpClients.createDefault())

  def clientSSL(sslcsf:SSLConnectionSocketFactory = DEFAULT_SSL_CONNECTION_SOCKET_FACTORY,
                cm:PoolingHttpClientConnectionManager = DEFAULT_POOLING_HTTP_CLIENT_CONNECTION_MANAGER):ScaHttpClient = {
    ScaHttpClient(HttpClients.custom().setSSLSocketFactory(sslcsf).setConnectionManager(cm).setConnectionManagerShared(true).build())
  }

  def clientSSL(strategy:TrustStrategy):ScaHttpClient = clientSSL(getDefaultSSLConnectionSocketFactory(strategy))

}
