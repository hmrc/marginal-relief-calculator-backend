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

import calculator.{ FYConfig, FlatRateConfig, MarginalReliefConfig }
import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import cats.syntax.validated._
import com.typesafe.config.ConfigFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.Configuration

class CalculatorConfigParserSpec extends AnyFreeSpec with Matchers {
  "AppConfig tests" - {
    "calculatorConfig" - {
      "should parse empty config" in {
        val configuration = configurationFromStr("""
                                                   |appName = test
                                                   |calculator-config = {
                                                   | fy-configs = [
                                                   | ]
                                                   |}
                                                   |""".stripMargin)
        CalculatorConfigParser.parse(configuration) shouldBe List.empty[FYConfig].valid
      }
      "should parse config with flat rate only" in {
        val configuration = configurationFromStr("""
                                                   |appName = test
                                                   |calculator-config = {
                                                   | fy-configs = [
                                                   |   {
                                                   |     year = 2022
                                                   |     main-rate = 0.19
                                                   |   }
                                                   | ]
                                                   |}
                                                   |""".stripMargin)
        CalculatorConfigParser.parse(configuration) shouldBe List(FlatRateConfig(2022, 0.19)).valid
      }
      "should parse config with MR rate only" in {
        val configuration = configurationFromStr("""
                                                   |appName = test
                                                   |calculator-config = {
                                                   | fy-configs = [
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
        CalculatorConfigParser.parse(configuration) shouldBe List(
          MarginalReliefConfig(2023, 50000, 250000, 0.19, 0.25, 0.015)
        ).valid
      }
      "should parse config with both flat and MR rates" in {
        val configuration = configurationFromStr("""
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
        CalculatorConfigParser.parse(configuration) shouldBe List(
          FlatRateConfig(2022, 0.19),
          MarginalReliefConfig(2023, 50000, 250000, 0.19, 0.25, 0.015)
        ).valid
      }
      "should error when config has missing year" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     invalid = 2022
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)

        CalculatorConfigParser.parse(configuration) shouldBe Invalid(
          NonEmptyList.of(
            InvalidConfigError(
              "year is missing or invalid"
            ),
            InvalidConfigError(
              "main-rate is missing or invalid"
            )
          )
        )
      }

      "should error when config has missing main-rate for flat rate config" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = 2022
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)
        CalculatorConfigParser.parse(configuration) shouldBe InvalidConfigError(
          "main-rate is missing or invalid for year 2022"
        ).invalidNel
      }

      "should error when config has invalid attributes for flat rate config" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = 2022
                                 |     main-rate = 0.19
                                 |     lower-threshold = 50000
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)
        CalculatorConfigParser.parse(configuration) shouldBe InvalidConfigError(
          "Invalid config for year 2022. For flat rate year, you need to specify year and main-rate only. " +
            "For marginal relief year, you need to specify year, lower-threshold, upper-threshold, small-profit-rate, main-rate and marginal-relief-fraction"
        ).invalidNel
      }

      "should error when missing required attribute for marginal relief config - marginal-relief-fraction missing" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = 2023
                                 |     lower-threshold = 50000
                                 |     upper-threshold = 250000
                                 |     small-profit-rate = 0.19
                                 |     main-rate = 0.25
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)

        CalculatorConfigParser.parse(configuration) shouldBe InvalidConfigError(
          "Invalid config for year 2023. For flat rate year, you need to specify year and main-rate only. " +
            "For marginal relief year, you need to specify year, lower-threshold, upper-threshold, small-profit-rate, main-rate and marginal-relief-fraction"
        ).invalidNel
      }

      "should error when invalid type for mandatory attribute - year should be number" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = "abc"
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)

        CalculatorConfigParser.parse(configuration) shouldBe Invalid(
          NonEmptyList.of(
            InvalidConfigError(
              "year is missing or invalid"
            ),
            InvalidConfigError(
              "main-rate is missing or invalid"
            )
          )
        )
      }

      "should error when invalid type for optional attribute - marginal-relief-fraction should be number" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = 2023
                                 |     lower-threshold = 50000
                                 |     upper-threshold = 250000
                                 |     small-profit-rate = 0.19
                                 |     main-rate = 0.25
                                 |     marginal-relief-fraction = "invalid"
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)

        CalculatorConfigParser.parse(configuration) shouldBe Invalid(
          NonEmptyList.of(
            InvalidConfigError("marginal-relief-fraction is invalid for year 2023")
          )
        )
      }

      "should error when there are duplicate years" in {
        val configuration =
          configurationFromStr("""
                                 |appName = test
                                 |calculator-config = {
                                 | fy-configs = [
                                 |   {
                                 |     year = 2022
                                 |     main-rate = 0.25
                                 |   },
                                 |   {
                                 |     year = 2023
                                 |     main-rate = 0.26
                                 |   },
                                 |   {
                                 |     year = 2023
                                 |     main-rate = 0.26
                                 |   },
                                 |   {
                                 |     year = 2024
                                 |     main-rate = 0.27
                                 |   },
                                 |   {
                                 |     year = 2024
                                 |     main-rate = 0.27
                                 |   }
                                 | ]
                                 |}
                                 |""".stripMargin)

        CalculatorConfigParser.parse(configuration) shouldBe Invalid(
          NonEmptyList.of(
            InvalidConfigError("Duplicate config found for years 2023,2024")
          )
        )
      }
    }
  }

  def configurationFromStr(configStr: String): Configuration =
    Configuration(ConfigFactory.parseString(configStr).withFallback(ConfigFactory.load()))
}
