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
import java.util.UUID

import cats.data.OptionT
import cats.effect.IO
import com.softwaremill.sttp._
import com.tle.core.httpclient._
import com.tle.core.db._
import com.tle.core.oauthclient.{OAuthClientService, OAuthTokenState}

object CloudProviderService {

  val OAuthServiceId = "oauth"

  def tokenUrlForProvider(cp: CloudProviderInstance): IO[Uri] = {
    cp.serviceUris
      .get(OAuthServiceId)
      .map { oauthService =>
        IO.fromEither(UriTemplateService.replaceVariables(oauthService.uri, cp.baseUrl, Map()))
      }
      .getOrElse(IO.raiseError(new Throwable("No OAuth service URL")))
  }

  def proxyRequest(uuid: UUID, serviceId: String): DB[String] = {
    CloudProviderDB
      .get(uuid)
      .flatMap { cp =>
        OptionT.fromOption[DB](cp.serviceUris.get(serviceId)).semiflatMap {
          case ServiceUri(uriTemplate, auth) =>
            for {
              uri <- dbLiftIO.liftIO {
                IO.fromEither(UriTemplateService.replaceVariables(uriTemplate, cp.baseUrl, Map()))
              }
              req = sttp.get(uri)
              response <- if (auth) {
                dbLiftIO.liftIO(tokenUrlForProvider(cp)).flatMap { oauthUrl =>
                  OAuthClientService.authorizedRequest(oauthUrl.toString,
                                                       cp.providerAuth.clientId,
                                                       cp.providerAuth.clientSecret,
                                                       req)
                }
              } else dbLiftIO.liftIO(req.send())
            } yield response.body.fold(identity, identity)
        }
      }
      .getOrElse("No such cloud provider")
  }

}
