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

import calculator.{ CalculatorConfig, FlatRateConfig, MarginalReliefConfig }
import com.typesafe.config.ConfigFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.Configuration

import scala.util.Try

class AppConfigSpec extends AnyFreeSpec with Matchers {

  "AppConfig tests" - {
    "calculatorConfig" - {
      "should parse config successfully" in {
        val appConfig = appConfigFromStr("""
                                           |appName = test
                                           |calculator-config = {
                                           | fy-configs = [
                                           |   {
                                           |     year = 2022
                                           |     main-rate = 0.19
                                           |   },
                                           |   {
                                           |     year = 2023
                                           |     lower-threshold = 50000
                                           |     upper-threshold = 250000
                                           |     small-profit-rate = 0.19
                                           |     main-rate = 0.25
                                           |     marginal-relief-fraction = 0.015
                                           |   }
                                           | ]
                                           |}
                                           |""".stripMargin)
        appConfig.calculatorConfig shouldBe CalculatorConfig(
          Seq(
            FlatRateConfig(2022, 0.19),
            MarginalReliefConfig(2023, 50000, 250000, 0.19, 0.25, 0.015)
          )
        )
      }
      "should error when config is invalid" in {
        val result = Try {
          appConfigFromStr("""
                             |appName = test
                             |calculator-config = {
                             | fy-configs = [
                             |   {
                             |     invalid = 2022
                             |   }
                             | ]
                             |}
                             |""".stripMargin)
        }
        result.isFailure shouldBe true
        result.failed.get.getMessage shouldBe "Failed to parse calculator-config"
      }
    }
  }

  def appConfigFromStr(configStr: String): AppConfig =
    new AppConfig(Configuration(ConfigFactory.parseString(configStr).withFallback(ConfigFactory.load())))
}
