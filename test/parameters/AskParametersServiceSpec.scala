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

package parameters

import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import cats.syntax.validated._
import com.typesafe.config.ConfigFactory
import config.AppConfig
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.Configuration

import java.time.LocalDate

class AskParametersServiceSpec extends AnyFreeSpec with Matchers {

  "associatedCompaniesParameters" - {

    "when no config exists for single year, returns error" in {
      val requiredParametersService = buildRequiredParametersService("""
                                                                       |appName = test
                                                                       |calculator-config = {
                                                                       | fy-configs = []
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2023, 4, 1),
        LocalDate.of(2024, 3, 31),
        0,
        None
      ) shouldBe ConfigMissingError(2023).invalidNel
    }

    "when accounting period within a Marginal Rate financial year, returns OnePeriod result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2023, 4, 1),
        LocalDate.of(2024, 3, 31),
        0,
        None
      ) shouldBe AskFull.validNel
    }

    "when no config exists for accounting period spanning financial years, returns error" in {
      val requiredParametersService = buildRequiredParametersService("""
                                                                       |appName = test
                                                                       |calculator-config = {
                                                                       | fy-configs = []
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2023, 1, 1),
        LocalDate.of(2023, 12, 31),
        0,
        None
      ) shouldBe Invalid(NonEmptyList.of(ConfigMissingError(2022), ConfigMissingError(2023)))
    }

    "when accounting period within a Flat Rate financial year, returns NotRequired result" in {
      val requiredParametersService = buildRequiredParametersService("""
                                                                       |appName = test
                                                                       |calculator-config = {
                                                                       | fy-configs = [
                                                                       |   {
                                                                       |     year = 2022
                                                                       |     main-rate = 0.25
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2022, 4, 1),
        LocalDate.of(2023, 3, 31),
        0,
        None
      ) shouldBe DontAsk.validNel
    }

    "when accounting period spans Flat Rate years, returns NotRequired result" in {
      val requiredParametersService = buildRequiredParametersService("""
                                                                       |appName = test
                                                                       |calculator-config = {
                                                                       | fy-configs = [
                                                                       |   {
                                                                       |     year = 2022
                                                                       |     main-rate = 0.25
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2023
                                                                       |     main-rate = 0.25
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2023, 1, 1),
        LocalDate.of(2023, 12, 31),
        0,
        None
      ) shouldBe DontAsk.validNel
    }

    "when accounting period spans Marginal Rate years and there is no change in thresholds, returns RequiresFull result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2024
                                                                       |     lower-threshold = 50000
                                                                       |     upper-threshold = 250000
                                                                       |     small-profit-rate = 0.19
                                                                       |     main-rate = 0.25
                                                                       |     marginal-relief-fraction = 0.015
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 31),
        0,
        None
      ) shouldBe AskFull.validNel
    }

    "when accounting period spans Marginal Rate years and there is change in upper threshold, returns RequiresBothParts result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2024
                                                                       |     lower-threshold = 50000
                                                                       |     upper-threshold = 300000
                                                                       |     small-profit-rate = 0.19
                                                                       |     main-rate = 0.25
                                                                       |     marginal-relief-fraction = 0.015
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 31),
        0,
        None
      ) shouldBe AskBothParts(
        Period(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 3, 31)),
        Period(LocalDate.of(2024, 4, 1), LocalDate.of(2024, 12, 31))
      ).validNel
    }

    "when accounting period spans Marginal Rate years and there is change in lower threshold, returns RequiresBothParts result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2024
                                                                       |     lower-threshold = 60000
                                                                       |     upper-threshold = 250000
                                                                       |     small-profit-rate = 0.19
                                                                       |     main-rate = 0.25
                                                                       |     marginal-relief-fraction = 0.015
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 31),
        0,
        None
      ) shouldBe AskBothParts(
        Period(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 3, 31)),
        Period(LocalDate.of(2024, 4, 1), LocalDate.of(2024, 12, 31))
      ).validNel
    }

    "when accounting period spans Marginal Rate years and there is change in both thresholds, returns RequiresBothParts result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2024
                                                                       |     lower-threshold = 60000
                                                                       |     upper-threshold = 300000
                                                                       |     small-profit-rate = 0.19
                                                                       |     main-rate = 0.25
                                                                       |     marginal-relief-fraction = 0.015
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 31),
        0,
        None
      ) shouldBe AskBothParts(
        Period(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 3, 31)),
        Period(LocalDate.of(2024, 4, 1), LocalDate.of(2024, 12, 31))
      ).validNel
    }

    "when accounting period spans Flat Rate and Marginal Rate years, returns OnePeriod result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2023, 1, 1),
        LocalDate.of(2023, 12, 31),
        0,
        None
      ) shouldBe AskOnePart(Period(LocalDate.of(2023, 4, 1), LocalDate.of(2023, 12, 31))).validNel
    }

    "when accounting period spans Marginal Rate and Flat Rate years, returns OnePeriod result" in {
      val requiredParametersService = buildRequiredParametersService("""
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
                                                                       |   },
                                                                       |   {
                                                                       |     year = 2024
                                                                       |     main-rate = 0.19
                                                                       |   }
                                                                       | ]
                                                                       |}
                                                                       |""".stripMargin)
      requiredParametersService.associatedCompaniesParameters(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 31),
        0,
        None
      ) shouldBe AskOnePart(Period(LocalDate.of(2024, 1, 1), LocalDate.of(2024, 3, 31))).validNel
    }
  }

  private def buildRequiredParametersService(config: String): AskParametersService =
    new AskParametersServiceImpl(appConfigFromStr(config))

  private def appConfigFromStr(configStr: String): AppConfig =
    new AppConfig(Configuration(ConfigFactory.parseString(configStr).withFallback(ConfigFactory.load())))

}
