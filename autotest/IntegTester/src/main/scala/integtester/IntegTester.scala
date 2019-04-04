/**
  * Created by jolz on 26/04/17.
  */
package integtester

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.staticcontent._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext

object IntegTester extends IOApp with Http4sDsl[IO] {

  val Logger = LoggerFactory.getLogger("IntegTester")

  val integDocument = {
    val inpStream = getClass.getResourceAsStream(s"/www/integtester.html")
    val htmlDoc   = Jsoup.parse(inpStream, "UTF-8", "")
    inpStream.close()
    htmlDoc
  }

  def appHtml(request: Request[IO], dir: String = ""): IO[Response[IO]] = request.decode[UrlForm] {
    form =>
      val formJson =
        (form.values.mapValues(_.toVector) ++ request.uri.query.multiParams).asJson.noSpaces
      val doc = integDocument.clone()
      doc.body().insertChildren(0, new Element("script").text(s"var postValues = $formJson"))
      Ok(doc.toString, `Content-Type`(MediaType.text.html))
  }

  def echoServer(request: Request[IO]): IO[Response[IO]] = {
    val echo = request.uri.query.params.get("echo")
    Ok(
      s"""<html>
      <head>
        <link rel="stylesheet" href="styles.css" type="text/css">
          <title>Echo Server</title>
        </head>
        <body>
            <div class="formrow">
              <label>
                Text to echo:
              </label>
              <span id="toecho">

              </span>
            </div>

            <div class="formrow">
              <label>
                Echoed:
              </label>
              <span id="echoed">${echo.getOrElse("")}</span>
            </div>
        </body>
      </html>""",
      `Content-Type`(MediaType.text.html)
    )
  }

  val appService = HttpRoutes.of[IO] {
    case request @ (GET | POST) -> Root / "index.html"        => appHtml(request)
    case request @ (GET | POST) -> Root / "echo" / "index.do" => echoServer(request)
  }

  def stream(args: List[String]) =
    BlazeBuilder[IO]
      .bindHttp(8083, "0.0.0.0")
      .mountService(appService, "")
      .mountService(TestingCloudProvider.oauthService, "/provider")
      .mountService(resourceService[IO](ResourceService.Config("/www", ExecutionContext.global)),
                    "/")
      .serve

  lazy val embeddedRunning: Boolean = {
    stream(List.empty).compile.drain.unsafeRunAsync(_ => ())
    true
  }

  def integTesterUrl: String = {
    embeddedRunning
    "http://localhost:8083/index.html"
  }

  def echoServerUrl: String = {
    embeddedRunning
    "http://localhost:8083/echo"
  }

  def providerRegistrationUrl: String = {
    embeddedRunning
    "http://localhost:8083/provider.html"
  }

  override def run(args: List[String]): IO[ExitCode] =
    stream(args).compile.drain.map(_ => ExitCode.Success)
}
