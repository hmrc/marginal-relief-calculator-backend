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

import calculator.DateUtils.{ financialYearEnd, _ }
import calculator.{ CalculatorConfig, FYConfig, FlatRateConfig, MarginalReliefConfig }
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import com.google.inject.{ ImplementedBy, Inject, Singleton }
import config.AppConfig

import java.time.LocalDate

@ImplementedBy(classOf[RequiredParametersServiceImpl])
trait RequiredParametersService {

  type ValidationResult[A] = ValidatedNel[ParameterError, A]

  def associatedCompaniesParameters(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: Double,
    exemptDistributions: Option[Double]
  ): ValidationResult[AssociatedCompaniesRequirement]
}

@Singleton
class RequiredParametersServiceImpl @Inject() (appConfig: AppConfig) extends RequiredParametersService {

  private val config: CalculatorConfig = appConfig.calculatorConfig

  override def associatedCompaniesParameters(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: Double,
    exemptDistributions: Option[Double]
  ): ValidationResult[AssociatedCompaniesRequirement] = {
    val fyEndForAccountingPeriodStart: LocalDate = financialYearEnd(accountingPeriodStart)
    if (fyEndForAccountingPeriodStart.isEqualOrAfter(accountingPeriodEnd)) {
      val fy = fyEndForAccountingPeriodStart.minusYears(1).getYear
      val maybeFYConfig = findFYConfig(fy)
      maybeFYConfig.map {
        case _: FlatRateConfig =>
          NotRequired
        case _: MarginalReliefConfig =>
          OnePeriod(Period(accountingPeriodStart, accountingPeriodEnd))
      }
    } else {
      val fy1 = fyEndForAccountingPeriodStart.minusYears(1).getYear
      val fy2 = fyEndForAccountingPeriodStart.getYear
      val maybeFY1Config = findFYConfig(fy1)
      val maybeFY2Config = findFYConfig(fy2)

      (maybeFY1Config, maybeFY2Config).mapN {
        case (_: FlatRateConfig, _: FlatRateConfig) =>
          NotRequired
        case (_: MarginalReliefConfig, _: MarginalReliefConfig) =>
          TwoPeriods(
            Period(accountingPeriodStart, fyEndForAccountingPeriodStart),
            Period(fyEndForAccountingPeriodStart.plusDays(1), accountingPeriodEnd)
          )
        case (_: FlatRateConfig, _: MarginalReliefConfig) =>
          OnePeriod(Period(fyEndForAccountingPeriodStart.plusDays(1), accountingPeriodEnd))
        case (_: MarginalReliefConfig, _: FlatRateConfig) =>
          OnePeriod(Period(accountingPeriodStart, fyEndForAccountingPeriodStart))
      }
    }
  }

  private def findFYConfig(year: Int): ValidationResult[FYConfig] =
    config.fyConfigs.sortBy(_.year)(Ordering[Int].reverse).find(_.year <= year) match {
      case Some(value) => value.validNel
      case None        => ConfigMissingError(year).invalidNel
    }
}
