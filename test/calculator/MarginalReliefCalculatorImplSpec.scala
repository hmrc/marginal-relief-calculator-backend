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

package calculator

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import config.AppConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration

import java.time.LocalDate

class MarginalReliefCalculatorImplSpec extends AnyWordSpec with Matchers {

  "compute" when {

    "config missing" should {

      "when single year and config missing, return error" in {
        val marginalReliefCalculator = new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                                                           |appName = test
                                                                                           |calculator-config = {
                                                                                           | fy-configs = [
                                                                                           | ]
                                                                                           |}
                                                                                           |""".stripMargin))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2022, 4, 1),
          LocalDate.of(2023, 3, 31),
          1,
          0,
          None,
          None,
          None
        )
        result shouldBe Left(ConfigMissingError(NonEmptyList.one(2022)))
      }

      "when straddles two financial years and both year configs missing, return error" in {
        val marginalReliefCalculator = new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                                                           |appName = test
                                                                                           |calculator-config = {
                                                                                           | fy-configs = []
                                                                                           |}
                                                                                           |""".stripMargin))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 1, 1),
          LocalDate.of(2023, 12, 31),
          1,
          0,
          None,
          None,
          None
        )
        result shouldBe Left(ConfigMissingError(NonEmptyList.of(2022, 2023)))
      }
    }

    "accounting period falls in a single financial year" should {

      "when config is missing for an accounting period year, fallback to the nearest config" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                              |appName = test
                                                              |calculator-config = {
                                                              | fy-configs = [
                                                              |   {
                                                              |     year = 2022
                                                              |     main-rate = 0.1
                                                              |   },
                                                              |   {
                                                              |     year = 2023
                                                              |     main-rate = 0.2
                                                              |   }
                                                              | ]
                                                              |}
                                                              |""".stripMargin))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2024, 4, 1),
          LocalDate.of(2025, 3, 31),
          100000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(20000.0, 20.0, 20000.0, 20.0, 0))
      }

      "when account period falls in FY with only main rate, apply main rate with no marginal relief" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                              |appName = test
                                                              |calculator-config = {
                                                              | fy-configs = [
                                                              |   {
                                                              |     year = 2022
                                                              |     main-rate = 0.19
                                                              |   }
                                                              | ]
                                                              |}
                                                              |""".stripMargin))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2022, 4, 1),
          LocalDate.of(2023, 3, 31),
          100000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(19000.0, 19.0, 19000.0, 19.0, 0))
      }
      "when account period falls in FY with marginal relief and profits are above the upper threshold, apply main rate" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 4, 1),
          LocalDate.of(2024, 3, 31),
          300000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(75000.0, 25.0, 75000.0, 25.0, 0))
      }
      "when account period falls in FY with marginal relief and profits are matching lower threshold, apply small profits rate" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 4, 1),
          LocalDate.of(2024, 3, 31),
          50000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(9500.0, 19.0, 9500.0, 19.0, 0))
      }
      "when account period falls in FY with marginal relief and profits are below lower threshold, apply small profits rate" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 4, 1),
          LocalDate.of(2024, 3, 31),
          40000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(7600.0, 19.0, 7600.0, 19.0, 0))
      }
      "when account period falls in FY with marginal relief and profits are between upper and lower thresholds, apply main rate with marginal relief" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 4, 1),
          LocalDate.of(2024, 3, 31),
          100000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(25000.0, 25.0, 22750.0, 22.75, 2250.0))
      }

      "when account period falls in FY with marginal relief and profits are between upper and lower thresholds and there are associated companies, apply main rate with marginal relief" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 4, 1),
          LocalDate.of(2024, 3, 31),
          100000,
          0,
          Some(1),
          None,
          None
        )
        result shouldBe Right(SingleResult(25000.0, 25.0, 24625.0, 24.63, 375.0))
      }
    }
    "accounting period straddles financial period" should {

      "when config is empty, return error" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                              |appName = test
                                                              |calculator-config = {
                                                              | fy-configs = [
                                                              | ]
                                                              |}
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 1, 1),
          LocalDate.of(2023, 12, 31),
          10000,
          0,
          None,
          None,
          None
        )
        result shouldBe Left(ConfigMissingError(NonEmptyList.of(2022, 2023)))
      }

      "when config missing for one of the years, fallback to the nearest config for the missing year" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2024, 1, 1),
          LocalDate.of(2024, 12, 31),
          10000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2023, 472.4, 19.0, 472.4, 19.0, 0.0),
            MarginalReliefByYear(2024, 1427.6, 19.0, 1427.6, 19.0, 0.0),
            19.0,
            19.0
          )
        )
      }

      "when config missing for both years, fallback to the nearest config" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2025, 1, 1),
          LocalDate.of(2025, 12, 31),
          10000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2024, 468.49, 19.0, 468.49, 19.0, 0.0),
            MarginalReliefByYear(2025, 1431.51, 19.0, 1431.51, 19.0, 0.0),
            19.0,
            19.0
          )
        )
      }

      "when no associated companies throughout the accounting period, no change in rates or thresholds" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2024, 1, 1),
          LocalDate.of(2024, 6, 30),
          25000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2023, 3125.0, 25.0, 2377.57, 19.02, 747.43),
            MarginalReliefByYear(2024, 3125.0, 25.0, 2377.57, 19.02, 747.43),
            25.0,
            19.02
          )
        )
      }
      "when FY1 with MR rate and FY2 with flat rate and associated companies in FY1, profits within threshold for FY1" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 6, 1),
          LocalDate.of(2024, 5, 31),
          70000,
          0,
          None,
          Some(2),
          None
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2023, 14583.33, 25.0, 14416.67, 24.71, 166.67),
            MarginalReliefByYear(2024, 2216.67, 19.0, 2216.67, 19.0, 0.0),
            24.0,
            23.76
          )
        )
      }
      "when FY1 with MR rate and FY2 with MR rate (no change in config), associated companies in (FY1, FY2) and profits within thresholds" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |   },
                                                              | ]
                                                              |}
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 10, 1),
          LocalDate.of(2024, 9, 30),
          60000,
          0,
          None,
          Some(2),
          Some(3)
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2023, 7500.0, 25.0, 7481.25, 24.94, 18.75),
            MarginalReliefByYear(2024, 7500.0, 25.0, 7481.25, 24.94, 18.75),
            25.0,
            24.94
          )
        )
      }
      "when FY1 with MR rate and FY2 with MR rate (change in config), associated companies in (FY1, FY2) and profits within thresholds" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                              |   },
                                                              | ]
                                                              |}
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 10, 1),
          LocalDate.of(2024, 9, 30),
          60000,
          0,
          None,
          Some(2),
          Some(3)
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2023, 7500.0, 25.0, 7325.0, 24.42, 175.0),
            MarginalReliefByYear(2024, 7500.0, 25.0, 7385.96, 24.62, 114.04),
            25.0,
            24.52
          )
        )
      }
      "when 2 associated companies in second accounting period, FY1 with flat rate and FY2 with MR rate - FY2 profits above upper threshold" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                              |appName = test
                                                              |calculator-config = {
                                                              | fy-configs = [
                                                              |   {
                                                              |     year = 2022
                                                              |     main-rate = 0.19
                                                              |   }
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
                                                              |""".stripMargin))

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 1, 1),
          LocalDate.of(2023, 12, 31),
          175000,
          0,
          None,
          None,
          Some(2)
        )
        result shouldBe Right(
          DualResult(
            MarginalReliefByYear(2022, 8198.63, 19.0, 8198.63, 19.0, 0),
            MarginalReliefByYear(2023, 32962.33, 25.0, 32962.33, 25.0, 0),
            23.52,
            23.52
          )
        )
      }
    }
    "when 2 associated in the second accounting period, FY1 with flat rate and FY2 with MR rate - FY2 profits between thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                            |appName = test
                                                            |calculator-config = {
                                                            | fy-configs = [
                                                            |   {
                                                            |     year = 2022
                                                            |     main-rate = 0.19
                                                            |   }
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
                                                            |""".stripMargin))

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2023, 1, 1),
        LocalDate.of(2023, 12, 31),
        60000,
        0,
        None,
        None,
        Some(2)
      )
      result shouldBe Right(
        DualResult(
          MarginalReliefByYear(2022, 2810.96, 19.0, 2810.96, 19.0, 0.0),
          MarginalReliefByYear(2023, 11301.37, 25.0, 11037.67, 24.42, 263.70),
          23.52,
          23.08
        )
      )
    }
    "when no of associated companies changes, no change in rates or thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                            |""".stripMargin))

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2023, 10, 1),
        LocalDate.of(2024, 9, 30),
        55000,
        0,
        Some(3),
        None,
        None
      )
      result shouldBe Right(
        DualResult(
          MarginalReliefByYear(2023, 6875.0, 25.0, 6818.75, 24.8, 56.25),
          MarginalReliefByYear(2024, 6875.0, 25.0, 6818.75, 24.8, 56.25),
          25.0,
          24.8
        )
      )
    }
    "when no of associated companies and rates change, no change in thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                            |appName = test
                                                            |calculator-config = {
                                                            | fy-configs = [
                                                            |   {
                                                            |     year = 2027
                                                            |     lower-threshold = 50000
                                                            |     upper-threshold = 250000
                                                            |     small-profit-rate = 0.19
                                                            |     main-rate = 0.25
                                                            |     marginal-relief-fraction = 0.015
                                                            |   },
                                                            |   {
                                                            |     year = 2028
                                                            |     lower-threshold = 50000
                                                            |     upper-threshold = 250000
                                                            |     small-profit-rate = 0.20
                                                            |     main-rate = 0.26
                                                            |     marginal-relief-fraction = 0.015
                                                            |   }
                                                            | ]
                                                            |}
                                                            |""".stripMargin))

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2028, 1, 1),
        LocalDate.of(2028, 12, 31),
        45000,
        0,
        Some(4),
        None,
        None
      )
      result shouldBe Right(
        DualResult(
          MarginalReliefByYear(2027, 2797.13, 25.0, 2778.48, 24.83, 18.65),
          MarginalReliefByYear(2028, 8790.98, 26.0, 8734.63, 25.83, 56.35),
          25.75,
          25.58
        )
      )
    }
    "when no of associated companies and thresholds change, no change in rates" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(appConfigFromStr("""
                                                            |appName = test
                                                            |calculator-config = {
                                                            | fy-configs = [
                                                            |   {
                                                            |     year = 2030
                                                            |     lower-threshold = 50000
                                                            |     upper-threshold = 250000
                                                            |     small-profit-rate = 0.19
                                                            |     main-rate = 0.25
                                                            |     marginal-relief-fraction = 0.015
                                                            |   },
                                                            |   {
                                                            |     year = 2031
                                                            |     lower-threshold = 50000
                                                            |     upper-threshold = 300000
                                                            |     small-profit-rate = 0.19
                                                            |     main-rate = 0.25
                                                            |     marginal-relief-fraction = 0.012
                                                            |   }
                                                            | ]
                                                            |}
                                                            |""".stripMargin))

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2030, 7, 1),
        LocalDate.of(2031, 6, 30),
        85000,
        0,
        None,
        Some(2),
        Some(1)
      )
      result shouldBe Right(
        DualResult(
          MarginalReliefByYear(2030, 15952.05, 25.0, 15952.05, 25.0, 0.0),
          MarginalReliefByYear(2031, 5297.95, 25.0, 5104.71, 24.09, 193.24),
          25.0,
          24.77
        )
      )
    }
    "when rates change, no change in thresholds and accounting period is 365 days" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(appConfigFromStr("""
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
                                                            |     main-rate = 0.26
                                                            |     marginal-relief-fraction = 0.015
                                                            |   }
                                                            | ]
                                                            |}
                                                            |""".stripMargin))

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2024, 1, 1),
        LocalDate.of(2024, 12, 30),
        100000,
        0,
        None,
        None,
        None
      )
      result shouldBe Right(
        DualResult(
          MarginalReliefByYear(2023, 6232.88, 25.0, 5671.92, 22.75, 560.96),
          MarginalReliefByYear(2024, 19517.81, 26.0, 17828.77, 23.75, 1689.04),
          25.75,
          23.5
        )
      )
    }
  }

  def appConfigFromStr(configStr: String): AppConfig =
    new AppConfig(Configuration(ConfigFactory.parseString(configStr).withFallback(ConfigFactory.load())))
}
