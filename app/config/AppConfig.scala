/*
 * Copyright 2022 HM Revenue & Customs
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

package config

import calculator.CalculatorConfig
import org.slf4j.{ Logger, LoggerFactory }
import play.api.Configuration

import javax.inject.{ Inject, Singleton }

@Singleton
class AppConfig @Inject() (config: Configuration) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  val appName: String = config.get[String]("appName")

  val calculatorConfig: CalculatorConfig =
    CalculatorConfigParser
      .parse(config)
      .fold(
        invalidConfigError => {
          val errors = invalidConfigError.map(_.message).toList.mkString(", ")
          logger.error(s"Failed to parse calculator-config. Errors are '$errors'")
          throw new RuntimeException("Failed to parse calculator-config")
        },
        fyConfigs => CalculatorConfig(fyConfigs)
      )
}
