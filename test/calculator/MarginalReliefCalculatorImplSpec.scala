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
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class MarginalReliefCalculatorImplSpec extends AnyWordSpec with Matchers {

  "compute" when {

    "config missing" should {

      "when single year and config missing, return error" in {
        val marginalReliefCalculator = new MarginalReliefCalculatorImpl(CalculatorConfig(List.empty))
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

      "when straddles two financial years and year two config missing, return error" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(CalculatorConfig(List(FYConfig(2022, 0, 0, 0, 0.19, None))))
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 1, 1),
          LocalDate.of(2023, 12, 31),
          1,
          0,
          None,
          None,
          None
        )
        result shouldBe Left(ConfigMissingError(NonEmptyList.one(2023)))
      }

      "when straddles two financial years and year one config missing, return error" in {
        val marginalReliefCalculator = new MarginalReliefCalculatorImpl(
          CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
        )
        val result = marginalReliefCalculator.compute(
          LocalDate.of(2023, 1, 1),
          LocalDate.of(2023, 12, 31),
          1,
          0,
          None,
          None,
          None
        )
        result shouldBe Left(ConfigMissingError(NonEmptyList.one(2022)))
      }

      "when straddles two financial years and both year configs missing, return error" in {
        val marginalReliefCalculator = new MarginalReliefCalculatorImpl(CalculatorConfig(List.empty))
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
      "when account period falls in FY with only main rate, apply main rate with no marginal relief" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(CalculatorConfig(List(FYConfig(2022, 0, 0, 0, 0.19, None))))
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
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
          )

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
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
          )

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
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
          )

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
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
          )

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
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(List(FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
          )

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
      "when no associated companies throughout the accounting period, no change in rates or thresholds" in {
        // Calculation for a company with short AP
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(
              List(
                FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015)),
                FYConfig(2024, 50000, 250000, 0.19, 0.25, Some(0.015))
              )
            )
          )

        val result = marginalReliefCalculator.compute(
          LocalDate.of(2024, 1, 1),
          LocalDate.of(2024, 6, 30),
          25000,
          0,
          None,
          None,
          None
        )
        result shouldBe Right(SingleResult(6250.0, 25.0, 4755.14, 19.02, 1494.86))
      }
      "when 2 associated companies in second accounting period, FY1 with only main rate and FY2 with marginal relief - FY2 profits above upper threshold" in {
        val marginalReliefCalculator =
          new MarginalReliefCalculatorImpl(
            CalculatorConfig(
              List(
                FYConfig(2022, 0, 0, 0, 0.19, None),
                FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))
              )
            )
          )

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
    "when 2 associated in the second accounting period, FY1 with only main rate and FY2 with marginal relief - FY2 profits between thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(
          CalculatorConfig(
            List(FYConfig(2022, 0, 0, 0, 0.19, None), FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015)))
          )
        )

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
          MarginalReliefByYear(2023, 11301.37, 25.0, 11037.67, 24.42, 263.7),
          23.52,
          23.08
        )
      )
    }
    "when no of associated companies changes, no change in rates or thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(
          CalculatorConfig(
            List(
              FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015)),
              FYConfig(2024, 50000, 250000, 0.19, 0.25, Some(0.015))
            )
          )
        )

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2023, 10, 1),
        LocalDate.of(2024, 9, 30),
        55000,
        0,
        Some(3),
        None,
        None
      )
      result shouldBe Right(SingleResult(13750.0, 25.0, 13637.5, 24.8, 112.5))
    }
    "when no of associated companies and rates change, no change in thresholds" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(
          CalculatorConfig(
            List(
              FYConfig(2027, 50000, 250000, 0.19, 0.25, Some(0.015)),
              FYConfig(2028, 50000, 250000, 0.20, 0.26, Some(0.015))
            )
          )
        )

      val result = marginalReliefCalculator.compute(
        LocalDate.of(2028, 1, 1),
        LocalDate.of(2028, 12, 31),
        45000,
        0,
        Some(4),
        None,
        None
      )
      result shouldBe Right(SingleResult(11588.11, 25.75, 11513.11, 25.58, 75.0))
    }
    "when no of associated companies and thresholds change, no change in rates" in {
      val marginalReliefCalculator =
        new MarginalReliefCalculatorImpl(
          CalculatorConfig(
            List(
              FYConfig(2030, 50000, 250000, 0.19, 0.25, Some(0.015)),
              FYConfig(2031, 50000, 300000, 0.19, 0.25, Some(0.012))
            )
          )
        )

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
          MarginalReliefByYear(2031, 5297.95, 25.0, 5103.48, 24.08, 194.47),
          25.0,
          24.77
        )
      )
    }
  }
}
