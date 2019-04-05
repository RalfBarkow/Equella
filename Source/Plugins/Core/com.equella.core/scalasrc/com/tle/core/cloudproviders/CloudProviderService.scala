/*
 * Copyright 2017 Apereo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.tle.core.cloudproviders

import java.net.URI
import java.nio.ByteBuffer
import java.time.Instant
import java.util.UUID

import cats.data.OptionT
import cats.effect.IO
import com.softwaremill.sttp._
import com.tle.beans.cloudproviders.{
  CloudConfigOption,
  CloudControlConfig,
  CloudControlConfigType,
  CloudControlDefinition
}
import com.tle.core.cache.{Cacheable, DBCacheBuilder}
import com.tle.core.httpclient._
import com.tle.core.db._
import com.tle.core.oauthclient.{OAuthClientService, OAuthTokenState}
import fs2.Stream
import com.softwaremill.sttp.circe._
import cats.syntax.applicative._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

object CloudProviderService {

  implicit val typeEnc = Encoder.enumEncoder(CloudControlConfigType)
  implicit val typeDec = Decoder.enumDecoder(CloudControlConfigType)
  implicit val ccoEnc  = deriveEncoder[CloudConfigOption]
  implicit val ccoDec  = deriveDecoder[CloudConfigOption]
  implicit val enc     = deriveEncoder[CloudControlConfig]
  implicit val dec     = deriveDecoder[CloudControlConfig]

  val OAuthServiceId = "oauth"

  def tokenUrlForProvider(cp: CloudProviderInstance): IO[Uri] = {
    cp.serviceUris
      .get(OAuthServiceId)
      .map { oauthService =>
        IO.fromEither(UriTemplateService.replaceVariables(oauthService.uri, cp.baseUrl, Map()))
      }
      .getOrElse(IO.raiseError(new Throwable("No OAuth service URL")))
  }

  def serviceRequest[T](serviceUri: ServiceUri,
                        provider: CloudProviderInstance,
                        params: Map[String, String],
                        f: Uri => Request[T, Stream[IO, ByteBuffer]]): DB[Response[T]] =
    for {
      uri <- dbLiftIO.liftIO {
        IO.fromEither(UriTemplateService.replaceVariables(serviceUri.uri, provider.baseUrl, params))
      }
      req  = f(uri)
      auth = provider.providerAuth
      response <- if (serviceUri.authenticated) {
        dbLiftIO.liftIO(tokenUrlForProvider(provider)).flatMap { oauthUrl =>
          OAuthClientService
            .authorizedRequest(oauthUrl.toString, auth.clientId, auth.clientSecret, req)
        }
      } else dbLiftIO.liftIO(req.send())
    } yield response

  def proxyRequest(uuid: UUID, serviceId: String): DB[String] = {
    CloudProviderDB
      .get(uuid)
      .flatMap { cp =>
        OptionT
          .fromOption[DB](cp.serviceUris.get(serviceId))
          .semiflatMap { service =>
            serviceRequest(service, cp, Map(), sttp.get)
          }
          .map { response =>
            response.body.fold(identity, identity)
          }
      }
      .getOrElse("No such cloud provider")
  }

  object ControlListCache
      extends Cacheable[CloudProviderInstance, (Instant, Iterable[CloudControlDefinition])] {
    override def cacheId: String = "cloudControlLists"

    override def key(userContext: UserContext, v: CloudProviderInstance): String =
      s"${userContext.inst.getUniqueId}_${v.id}"

    def invalidData: (Instant, Iterable[CloudControlDefinition]) =
      Instant.now().plusSeconds(10) -> Iterable.empty

    override def query: CloudProviderInstance => DB[(Instant, Iterable[CloudControlDefinition])] =
      cp => {
        cp.serviceUris
          .get("controls")
          .map { controlsService =>
            dbAttempt {
              serviceRequest(
                controlsService,
                cp,
                Map(),
                u => sttp.get(u).response(asJson[Map[String, Iterable[CloudControlConfig]]])).map {
                res =>
                  res.body match {
                    case Right(Right(controlMap)) =>
                      val controls = controlMap.map {
                        case (controlId, config) =>
                          CloudControlDefinition(cp.id,
                                                 controlId,
                                                 controlId,
                                                 "/icons/control.gif",
                                                 config)
                      }
                      (Instant.now().plusSeconds(60), controls)
                    case o =>
                      invalidData
                  }
              }
            }.map(_.fold(_ => invalidData, identity))
          }
          .getOrElse {
            invalidData.pure[DB]
          }
      }
  }

  val controlListCache = DBCacheBuilder.buildCache(ControlListCache)

  def queryControls(): DB[Vector[CloudControlDefinition]] =
    CloudProviderDB.readAll
      .evalMap { cp =>
        controlListCache.getIfValid(cp, _._1.isBefore(Instant.now()))
      }
      .flatMap(c => Stream.emits(c._2.toSeq))
      .compile
      .toVector

}
