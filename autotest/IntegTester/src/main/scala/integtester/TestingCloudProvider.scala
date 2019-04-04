package integtester

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.circe._

import scalaoauth2.provider.{
  AccessToken,
  AuthInfo,
  AuthorizationHandler,
  AuthorizationRequest,
  ClientCredential,
  ClientCredentials,
  TokenEndpoint
}

import scala.concurrent.Future

class TestUser

object TestingCloudProvider
    extends Http4sDsl[IO]
    with TokenEndpoint
    with AuthorizationHandler[TestUser] {

  case class OAuthTokenResponse(access_token: String,
                                refresh_token: Option[String],
                                token_type: Option[String],
                                expires_in: Option[Long],
                                state: Option[String])

  override val handlers = Map("client_credentials" -> new ClientCredentials)

  import scala.concurrent.ExecutionContext.Implicits.global

  val oauthService = HttpRoutes.of[IO] {
    case request @ POST -> Root / "access_token" =>
      request.decode[UrlForm] { formData =>
        val headerMap = request.headers.groupBy(_.name.value).mapValues(_.map(_.value).toSeq)
        val authReq   = new AuthorizationRequest(headerMap, formData.values.mapValues(_.toVector))
        IO.fromFuture { IO(handleRequest(authReq, this)) }.map {
          case Left(err) =>
          case Right(result) =>
            Ok(
              OAuthTokenResponse(result.accessToken,
                                 result.refreshToken,
                                 Some(result.tokenType),
                                 expires_in = result.expiresIn,
                                 None))
        }
      }
  }

  override def validateClient(maybeCredential: Option[ClientCredential],
                              request: AuthorizationRequest): Future[Boolean] = {
    Future.successful(true)
  }

  override def findUser(maybeCredential: Option[ClientCredential],
                        request: AuthorizationRequest): Future[Option[TestUser]] = {
    Future.successful(None)
  }

  override def createAccessToken(authInfo: AuthInfo[TestUser]): Future[AccessToken] = {
    Future.successful(AccessToken("MYTOKEN", None, None, None, new java.util.Date(), Map.empty))
  }

  override def getStoredAccessToken(authInfo: AuthInfo[TestUser]): Future[Option[AccessToken]] = ???

  override def refreshAccessToken(authInfo: AuthInfo[TestUser],
                                  refreshToken: String): Future[AccessToken] = ???

  override def findAuthInfoByCode(code: String): Future[Option[AuthInfo[TestUser]]] = ???

  override def deleteAuthCode(code: String): Future[Unit] = ???

  override def findAuthInfoByRefreshToken(
      refreshToken: String): Future[Option[AuthInfo[TestUser]]] = ???
}
