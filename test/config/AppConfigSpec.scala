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

import calculator.{ CalculatorConfig, FYConfig }
import com.typesafe.config.ConfigFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.Configuration

import scala.util.Try

class AppConfigSpec extends AnyFreeSpec with Matchers {

  "AppConfig tests" - {
    "calculatorConfig" - {
      "should parse empty config" in {
        val appConfig = appConfigFromStr("""
                                           |appName = test
                                           |calculator-config = {
                                           | fyConfigs = [
                                           | ]
                                           |}
                                           |""".stripMargin)
        appConfig.calculatorConfig shouldBe CalculatorConfig(Seq.empty)
      }
      "should parse config with flat rate only" in {
        val appConfig = appConfigFromStr("""
                                           |appName = test
                                           |calculator-config = {
                                           | fyConfigs = [
                                           |   {
                                           |     year = 2022
                                           |     main-rate = 0.19
                                           |   }
                                           | ]
                                           |}
                                           |""".stripMargin)
        appConfig.calculatorConfig shouldBe CalculatorConfig(Seq(FYConfig(2022, None, None, None, 0.19, None)))
      }
      "should parse config with MR rate only" in {
        val appConfig = appConfigFromStr("""
                                           |appName = test
                                           |calculator-config = {
                                           | fyConfigs = [
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
          Seq(FYConfig(2023, Some(50000), Some(250000), Some(0.19), 0.25, Some(0.015)))
        )
      }
      "should parse config with both flat and MR rates" in {
        val appConfig = appConfigFromStr("""
                                           |appName = test
                                           |calculator-config = {
                                           | fyConfigs = [
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
            FYConfig(2022, None, None, None, 0.19, None),
            FYConfig(2023, Some(50000), Some(250000), Some(0.19), 0.25, Some(0.015))
          )
        )
      }
      "should error when config has missing mandatory attributes" in {
        val result = Try {
          appConfigFromStr("""
                             |appName = test
                             |calculator-config = {
                             | fyConfigs = [
                             |   {
                             |     year = 2022
                             |   }
                             | ]
                             |}
                             |""".stripMargin)
        }
        result.isFailure shouldBe true
        result.failed.get.getMessage shouldBe "String: 5: No configuration setting found for key 'main-rate'"
      }
    }
  }

  def appConfigFromStr(configStr: String): AppConfig =
    new AppConfig(Configuration(ConfigFactory.parseString(configStr).withFallback(ConfigFactory.load())))
}
