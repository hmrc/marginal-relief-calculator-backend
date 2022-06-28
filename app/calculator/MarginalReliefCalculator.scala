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

import calculator.DateUtils.{ daysBetweenInclusive, daysInFY, financialYearEnd, _ }
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import com.google.inject.{ ImplementedBy, Inject, Singleton }
import config.AppConfig

import java.time.LocalDate

@ImplementedBy(classOf[MarginalReliefCalculatorImpl])
trait MarginalReliefCalculator {

  type ValidationResult[A] = ValidatedNel[CalculatorError, A]

  def compute(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: BigDecimal,
    exemptDistributions: BigDecimal,
    associatedCompanies: Option[Int],
    associatedCompaniesFY1: Option[Int],
    associatedCompaniesFY2: Option[Int]
  ): ValidationResult[MarginalReliefResult]
}

@Singleton
class MarginalReliefCalculatorImpl @Inject() (appConfig: AppConfig) extends MarginalReliefCalculator {

  private val config: CalculatorConfig = appConfig.calculatorConfig

  override def compute(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: BigDecimal,
    exemptDistributions: BigDecimal,
    associatedCompanies: Option[Int],
    associatedCompaniesFY1: Option[Int],
    associatedCompaniesFY2: Option[Int]
  ): ValidationResult[MarginalReliefResult] = {

    val daysInAP: Int = daysBetweenInclusive(accountingPeriodStart, accountingPeriodEnd)
    val fyEndForAPStartDate: LocalDate = financialYearEnd(accountingPeriodStart)

    if (fyEndForAPStartDate.isEqualOrAfter(accountingPeriodEnd)) {
      // one financial year
      val fy = accountingPeriodStart.getYear
      val maybeFYConfig = findFYConfig(fy)
      maybeFYConfig.map {
        case flatRateConfig: FlatRateConfig =>
          val ct = BigDecimal(flatRateConfig.mainRate) * profit
          SingleResult(
            fy,
            roundUp(ct),
            roundUp((ct / profit) * 100),
            roundUp(ct),
            roundUp((ct / profit) * 100),
            roundUp(BigDecimal(0))
          )
        case marginalReliefConfig: MarginalReliefConfig =>
          val fyRatio = ratioForAdjustingThresholds(None, None, daysInAP, daysInFY(fy), daysInAP)
          val corporationTaxBeforeMR =
            computeCorporationTax(
              profit,
              profit + exemptDistributions,
              marginalReliefConfig.lowerThreshold,
              associatedCompanies.getOrElse(0) + 1,
              fyRatio,
              marginalReliefConfig.smallProfitRate,
              marginalReliefConfig.mainRate
            )
          val marginalRelief = computeMarginalRelief(
            profit,
            profit + exemptDistributions,
            associatedCompanies.getOrElse(0) + 1,
            marginalReliefConfig.lowerThreshold,
            marginalReliefConfig.upperThreshold,
            fyRatio,
            marginalReliefConfig.marginalReliefFraction
          )
          val corporationTax = roundUp(corporationTaxBeforeMR - marginalRelief)
          val effectiveRateBeforeMR = roundUp((corporationTaxBeforeMR / profit) * 100)
          val effectiveRate = roundUp((corporationTax / profit) * 100)

          SingleResult(
            fy,
            roundUp(corporationTaxBeforeMR),
            effectiveRateBeforeMR,
            corporationTax,
            effectiveRate,
            roundUp(marginalRelief)
          )
      }
    } else {
      // straddles financial years
      val fy1 = fyEndForAPStartDate.minusYears(1).getYear
      val fy2 = fyEndForAPStartDate.getYear

      val maybeFY1Config = findFYConfig(fy1)
      val maybeFY2Config = findFYConfig(fy2)

      val apDaysInFY1 = daysBetweenInclusive(accountingPeriodStart, fyEndForAPStartDate)
      val apDaysInFY2 = daysInAP - apDaysInFY1

      val apFY1Ratio = BigDecimal(apDaysInFY1) / daysInAP
      val apFY2Ratio = BigDecimal(apDaysInFY2) / daysInAP

      val adjustedProfitFY1 = profit * apFY1Ratio
      val adjustedExemptDistributionsFY1 = exemptDistributions * apFY1Ratio
      val adjustedAugmentedProfitFY1 = adjustedProfitFY1 + adjustedExemptDistributionsFY1

      val adjustedProfitFY2 = profit * apFY2Ratio
      val adjustedExemptDistributionsFY2 = exemptDistributions * apFY2Ratio
      val adjustedAugmentedProfitFY2 = adjustedProfitFY2 + adjustedExemptDistributionsFY2

      (maybeFY1Config, maybeFY2Config).mapN {
        case (fy1Config: FlatRateConfig, fy2Config: FlatRateConfig) =>
          val ctFY1 = BigDecimal(fy1Config.mainRate) * adjustedProfitFY1
          val ctFY2 = BigDecimal(fy2Config.mainRate) * adjustedProfitFY2
          DualResult(
            MarginalReliefByYear(
              fy1,
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(BigDecimal(0))
            ),
            MarginalReliefByYear(
              fy2,
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(BigDecimal(0))
            ),
            roundUp(((ctFY1 + ctFY2) / profit) * 100),
            roundUp(((ctFY1 + ctFY2) / profit) * 100)
          )
        case (fy1Config: MarginalReliefConfig, fy2Config: MarginalReliefConfig) =>
          val fy1Ratio = ratioForAdjustingThresholds(
            Some(fy1Config.upperThreshold),
            Some(fy2Config.upperThreshold),
            apDaysInFY1,
            daysInFY(fy1),
            daysInAP
          )
          val fy2Ratio = ratioForAdjustingThresholds(
            Some(fy1Config.upperThreshold),
            Some(fy2Config.upperThreshold),
            apDaysInFY2,
            daysInFY(fy2),
            daysInAP
          )

          val companiesFY1 = calculateCompanies(
            fy1Config,
            fy2Config,
            associatedCompanies,
            associatedCompaniesFY1,
            associatedCompaniesFY2,
            fy1Config.year
          )
          val companiesFY2 = calculateCompanies(
            fy1Config,
            fy2Config,
            associatedCompanies,
            associatedCompaniesFY1,
            associatedCompaniesFY2,
            fy2Config.year
          )

          val ctFY1 =
            computeCorporationTax(
              adjustedProfitFY1,
              adjustedAugmentedProfitFY1,
              fy1Config.lowerThreshold,
              companiesFY1,
              fy1Ratio,
              fy1Config.smallProfitRate,
              fy1Config.mainRate
            )
          val ctFY2 =
            computeCorporationTax(
              adjustedProfitFY2,
              adjustedAugmentedProfitFY2,
              fy2Config.lowerThreshold,
              companiesFY2,
              fy2Ratio,
              fy2Config.smallProfitRate,
              fy2Config.mainRate
            )
          val mr1 = computeMarginalRelief(
            adjustedProfitFY1,
            adjustedAugmentedProfitFY1,
            companiesFY1,
            fy1Config.lowerThreshold,
            fy1Config.upperThreshold,
            fy1Ratio,
            fy1Config.marginalReliefFraction
          )
          val mr2 = computeMarginalRelief(
            adjustedProfitFY2,
            adjustedAugmentedProfitFY2,
            companiesFY2,
            fy2Config.lowerThreshold,
            fy2Config.upperThreshold,
            fy2Ratio,
            fy2Config.marginalReliefFraction
          )

          DualResult(
            MarginalReliefByYear(
              fy1,
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(ctFY1 - mr1),
              roundUp(((ctFY1 - mr1) / adjustedProfitFY1) * 100),
              roundUp(mr1)
            ),
            MarginalReliefByYear(
              fy2,
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(ctFY2 - mr2),
              roundUp(((ctFY2 - mr2) / adjustedProfitFY2) * 100),
              roundUp(mr2)
            ),
            roundUp(((ctFY1 + ctFY2) / profit) * 100),
            roundUp(((ctFY1 - mr1 + ctFY2 - mr2) / profit) * 100)
          )
        case (fy1Config: FlatRateConfig, fy2Config: MarginalReliefConfig) =>
          val ctFY1 = BigDecimal(fy1Config.mainRate) * adjustedProfitFY1
          val fy2Ratio = ratioForAdjustingThresholds(
            None,
            Some(fy2Config.upperThreshold),
            apDaysInFY2,
            daysInFY(fy2),
            daysInAP
          )
          val accFY2 = associatedCompaniesFY2.getOrElse(0) + 1
          val ctFY2 =
            computeCorporationTax(
              adjustedProfitFY2,
              adjustedAugmentedProfitFY2,
              fy2Config.lowerThreshold,
              accFY2,
              fy2Ratio,
              fy2Config.smallProfitRate,
              fy2Config.mainRate
            )
          val mr2 = computeMarginalRelief(
            adjustedProfitFY2,
            adjustedAugmentedProfitFY2,
            accFY2,
            fy2Config.lowerThreshold,
            fy2Config.upperThreshold,
            fy2Ratio,
            fy2Config.marginalReliefFraction
          )
          DualResult(
            MarginalReliefByYear(
              fy1,
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(BigDecimal(0))
            ),
            MarginalReliefByYear(
              fy2,
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(ctFY2 - mr2),
              roundUp(((ctFY2 - mr2) / adjustedProfitFY2) * 100),
              roundUp(mr2)
            ),
            roundUp(((ctFY1 + ctFY2) / profit) * 100),
            roundUp(((ctFY1 + ctFY2 - mr2) / profit) * 100)
          )
        case (fy1Config: MarginalReliefConfig, fy2Config: FlatRateConfig) =>
          val fy1Ratio = ratioForAdjustingThresholds(
            Some(fy1Config.upperThreshold),
            None,
            apDaysInFY1,
            daysInFY(fy1),
            daysInAP
          )
          val accFY1 = associatedCompaniesFY1.getOrElse(0) + 1
          val ctFY1 =
            computeCorporationTax(
              adjustedProfitFY1,
              adjustedAugmentedProfitFY1,
              fy1Config.lowerThreshold,
              accFY1,
              fy1Ratio,
              fy1Config.smallProfitRate,
              fy1Config.mainRate
            )
          val mr1 = computeMarginalRelief(
            adjustedProfitFY1,
            adjustedAugmentedProfitFY1,
            accFY1,
            fy1Config.lowerThreshold,
            fy1Config.upperThreshold,
            fy1Ratio,
            fy1Config.marginalReliefFraction
          )
          val ctFY2 = BigDecimal(fy2Config.mainRate) * adjustedProfitFY2

          DualResult(
            MarginalReliefByYear(
              fy1,
              roundUp(ctFY1),
              roundUp((ctFY1 / adjustedProfitFY1) * 100),
              roundUp(ctFY1 - mr1),
              roundUp(((ctFY1 - mr1) / adjustedProfitFY1) * 100),
              roundUp(mr1)
            ),
            MarginalReliefByYear(
              fy2,
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(ctFY2),
              roundUp((ctFY2 / adjustedProfitFY2) * 100),
              roundUp(BigDecimal(0))
            ),
            roundUp(((ctFY1 + ctFY2) / profit) * 100),
            roundUp(((ctFY1 - mr1 + ctFY2) / profit) * 100)
          )
      }
    }
  }

  private def findFYConfig(year: Int): ValidationResult[FYConfig] =
    config.fyConfigs.sortBy(_.year)(Ordering[Int].reverse).find(_.year <= year) match {
      case Some(value) => value.validNel
      case None        => ConfigMissingError(year).invalidNel
    }

  def calculateCompanies(
    fy1Config: MarginalReliefConfig,
    fy2Config: MarginalReliefConfig,
    maybeAssociatedCompanies: Option[Int],
    maybeAssociatedCompaniesFY1: Option[Int],
    maybeAssociatedCompaniesFY2: Option[Int],
    forFYYear: Int
  ): Int =
    maybeAssociatedCompanies match {
      case Some(associatedCompanies) => associatedCompanies + 1
      case None =>
        (maybeAssociatedCompaniesFY1, maybeAssociatedCompaniesFY2) match {
          case (Some(associatedCompaniesFY1), None) => associatedCompaniesFY1 + 1
          case (None, Some(associatedCompaniesFY2)) => associatedCompaniesFY2 + 1
          case (Some(associatedCompaniesFY1), Some(associatedCompaniesFY2))
              if fy1Config.lowerThreshold == fy2Config.lowerThreshold && fy1Config.upperThreshold == fy2Config.upperThreshold =>
            Math.max(associatedCompaniesFY1, associatedCompaniesFY2) + 1
          case (Some(associatedCompaniesFY1), Some(associatedCompaniesFY2)) =>
            if (fy1Config.year == forFYYear) associatedCompaniesFY1 + 1 else associatedCompaniesFY2 + 1
          case (None, None) => 1
        }
    }

  private def computeCorporationTax(
    adjustedProfit: BigDecimal,
    adjustedAugmentedProfit: BigDecimal,
    lowerThreshold: Int,
    companies: Int,
    fyRatio: BigDecimal,
    smallProfitRate: Double,
    mainRate: Double
  ): BigDecimal = {
    val adjustedLT = BigDecimal(lowerThreshold) * fyRatio / BigDecimal(companies)
    // calculate corporation tax
    val corporationTax =
      adjustedProfit * (if (adjustedAugmentedProfit <= adjustedLT) BigDecimal(smallProfitRate)
                        else BigDecimal(mainRate))
    corporationTax
  }

  private def computeMarginalRelief(
    adjustedProfit: BigDecimal,
    adjustedAugmentedProfit: BigDecimal,
    companies: Int,
    lowerThreshold: Int,
    upperThreshold: Int,
    fyRatio: BigDecimal,
    marginalReliefFraction: Double
  ): BigDecimal = {
    // adjust upper and lower thresholds
    val adjustedLT = lowerThreshold * fyRatio / BigDecimal(companies)
    val adjustedUT = upperThreshold * fyRatio / BigDecimal(companies)
    // calculate marginal relief
    if (adjustedAugmentedProfit > adjustedLT && adjustedAugmentedProfit <= adjustedUT) {
      BigDecimal(
        marginalReliefFraction
      ) * (adjustedUT - adjustedAugmentedProfit) * (adjustedProfit / adjustedAugmentedProfit)
    } else {
      BigDecimal(0)
    }
  }

  private def ratioForAdjustingThresholds(
    maybeUpperThresholdFY1: Option[Int],
    maybeUpperThresholdFY2: Option[Int],
    apDaysInFY: Int,
    fyDays: Int,
    daysInAP: Int
  ): BigDecimal =
    (maybeUpperThresholdFY1, maybeUpperThresholdFY2) match {
      case (Some(upperThresholdFY1), Some(upperThresholdFy2))
          if upperThresholdFY1 != upperThresholdFy2 => // both needs to be MR years for comparison
        BigDecimal(apDaysInFY) / fyDays
      case _ => // flat rate year
        BigDecimal(apDaysInFY) / (if (daysInAP == 366) 366 else 365)
    }

  private def roundUp(value: BigDecimal): Double =
    value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
