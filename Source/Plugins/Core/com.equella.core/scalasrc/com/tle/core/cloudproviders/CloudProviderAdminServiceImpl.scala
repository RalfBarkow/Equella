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

import java.util
import java.util.UUID

import com.tle.beans.cloudproviders.{
  CloudConfigControl,
  CloudConfigOption,
  CloudControlDefinition,
  CloudControlType
}
import com.tle.core.guice.Bind
import com.tle.core.remoting.CloudProviderAdminService

import scala.collection.JavaConverters._

@Bind(classOf[CloudProviderAdminService])
class CloudProviderAdminServiceImpl extends CloudProviderAdminService {

  val multiOptions =
    Iterable(CloudConfigOption("First entry", "first"), CloudConfigOption("SecondEntry", "second"))

  override def listControls: util.List[CloudControlDefinition] = {
    val ctrl1 = CloudControlDefinition(
      UUID.fromString("36dc5aaa-b0d4-462c-b6c4-67b7f2f986a3"),
      "sample",
      "My control",
      "/icons/control.gif",
      Iterable(
        CloudConfigControl("path",
                           "Target node",
                           Some("Please select a target node"),
                           CloudControlType.XPath,
                           Iterable.empty,
                           0,
                           1),
        CloudConfigControl("plaintext",
                           "A text entry",
                           Some("This is some text"),
                           CloudControlType.Textfield,
                           Iterable.empty,
                           1,
                           1),
        CloudConfigControl("choice",
                           "A drop down",
                           Some("This is mandatory"),
                           CloudControlType.Dropdown,
                           multiOptions,
                           1,
                           1),
        CloudConfigControl("checkbox",
                           "Some checboxes",
                           Some("Select some if you want"),
                           CloudControlType.Check,
                           multiOptions,
                           0,
                           Int.MaxValue),
        CloudConfigControl("radio",
                           "Some radioboxes",
                           Some("Please select one of these"),
                           CloudControlType.Radio,
                           multiOptions,
                           1,
                           1)
      )
    )
    List(ctrl1: CloudControlDefinition).asJava
  }
}
