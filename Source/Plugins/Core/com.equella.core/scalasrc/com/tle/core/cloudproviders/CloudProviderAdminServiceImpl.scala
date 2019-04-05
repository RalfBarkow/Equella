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

import com.tle.beans.cloudproviders._
import com.tle.core.db.RunWithDB
import com.tle.core.guice.Bind
import com.tle.core.remoting.CloudProviderAdminService

import scala.collection.JavaConverters._

@Bind(classOf[CloudProviderAdminService])
class CloudProviderAdminServiceImpl extends CloudProviderAdminService {

  val multiOptions1 =
    Iterable(CloudConfigOption("Yes", "yes"), CloudConfigOption("No", "no"))

  val multiOptions2 =
    Iterable(CloudConfigOption("Bold", "bold"), CloudConfigOption("Italic", "italic"))

  val multiOptions3 =
    Iterable(CloudConfigOption("Red", "red"), CloudConfigOption("Black", "black"))

  override def listControls: util.List[CloudControlDefinition] = {
    RunWithDB.execute(CloudProviderService.queryControls()).asJava
//
//    val ctrl1 = CloudControlDefinition(
//      UUID.fromString("36dc5aaa-b0d4-462c-b6c4-67b7f2f986a3"),
//      "sample",
//      "My control",
//      "/icons/control.gif",
//      List(
//        CloudControlConfig("path",
//                           "Please select a target node",
//                           Some("Please select a target node"),
//                           CloudControlConfigType.XPath,
//                           Iterable.empty,
//                           0,
//                           1),
//        CloudControlConfig("plaintext",
//                           "Description",
//                            Some("This is some text"),
//                           CloudControlConfigType.Textfield,
//                           Iterable.empty,
//                           1,
//                           1),
//        CloudControlConfig("choice",
//                           "Display color",
//                           Some("This is mandatory"),
//                           CloudControlConfigType.Dropdown,
//                           multiOptions3,
//                           1,
//                           1),
//        CloudControlConfig("checkbox",
//                           "Display style",
//                           Some("Select some if you want"),
//                           CloudControlConfigType.Check,
//                           multiOptions2,
//                           0,
//                           Int.MaxValue),
//        CloudControlConfig("radio",
//                           "This item is mandatory",
//                           Some("Please select one of these"),
//                           CloudControlConfigType.Radio,
//                           multiOptions1,
//                           1,
//                           1)
//      )
//    )
//
//    List(ctrl1: CloudControlDefinition).asJava
  }
}
