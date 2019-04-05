package integtester

import java.util.UUID

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

package object testprovider {

  case class CloudConfigOption(name: String, value: String)

  object CloudControlConfigType extends Enumeration {
    val XPath, Textfield, Dropdown, Check, Radio = Value
  }

  case class CloudControlConfig(id: String,
                                name: String,
                                description: Option[String],
                                configType: CloudControlConfigType.Value,
                                options: Iterable[CloudConfigOption],
                                min: Int,
                                max: Int)

  object CloudControlConfig {
    implicit val typeEnc = Encoder.enumEncoder(CloudControlConfigType)
    implicit val typeDec = Decoder.enumDecoder(CloudControlConfigType)
    implicit val ccoEnc  = deriveEncoder[CloudConfigOption]
    implicit val ccoDec  = deriveDecoder[CloudConfigOption]
    implicit val enc     = deriveEncoder[CloudControlConfig]
    implicit val dec     = deriveDecoder[CloudControlConfig]
  }

  case class CloudControlDefinition(providerId: UUID,
                                    controlId: String,
                                    name: String,
                                    iconUrl: String,
                                    configDefinition: Iterable[CloudControlConfig])
}
