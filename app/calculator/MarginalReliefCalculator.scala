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
import cats.data.NonEmptyList.one
import com.google.inject.{ ImplementedBy, Inject, Singleton }
import config.AppConfig

import java.time.temporal.ChronoUnit
import java.time.{ LocalDate, Month }

@ImplementedBy(classOf[MarginalReliefCalculatorImpl])
trait MarginalReliefCalculator {
  def compute(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: BigDecimal,
    exemptDistributions: BigDecimal,
    associatedCompanies: Option[Int],
    associatedCompaniesFY1: Option[Int],
    associatedCompaniesFY2: Option[Int]
  ): Either[CalculatorError, MarginalReliefResult]
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
  ): Either[CalculatorError, MarginalReliefResult] = {

    val daysInAP: Int = daysBetweenInclusive(accountingPeriodStart, accountingPeriodEnd)
    val fyEndForAPStartDate: LocalDate = financialYearEnd(accountingPeriodStart)

    if (
      fyEndForAPStartDate
        .isEqual(accountingPeriodEnd) || fyEndForAPStartDate.isAfter(accountingPeriodEnd)
    ) {
      // one financial year
      val maybeFYConfig = config.fyConfigs.find(_.year == accountingPeriodStart.getYear)

      maybeFYConfig match {
        case Some(fyConfig) =>
          val fyRatio = BigDecimal(daysInAP) / (if (daysInAP == 366) 366 else 365)

          val CTWithAdjustments(
            _,
            _,
            _,
            _,
            corporationTaxBeforeMR
          ) =
            computeCorporationTax(
              profit,
              exemptDistributions,
              1,
              fyConfig.lowerThreshold,
              associatedCompanies.map(_ + 1),
              fyRatio,
              fyConfig.smallProfitRate,
              fyConfig.mainRate
            )
          val marginalRelief = computeMarginalRelief(
            profit,
            profit + exemptDistributions,
            associatedCompanies.map(_ + 1),
            fyConfig.lowerThreshold,
            fyConfig.upperThreshold,
            fyRatio,
            fyConfig.marginalReliefFraction
          )
          val corporationTax = roundUp(corporationTaxBeforeMR - marginalRelief)
          val effectiveRateBeforeMR = roundUp((corporationTaxBeforeMR / profit) * 100)
          val effectiveRate = roundUp((corporationTax / profit) * 100)

          Right(
            SingleResult(
              roundUp(corporationTaxBeforeMR),
              effectiveRateBeforeMR,
              corporationTax,
              effectiveRate,
              roundUp(marginalRelief)
            )
          )

        case None => Left(ConfigMissingError(one(accountingPeriodStart.getYear)))
      }
    } else {
      // straddles 2 financial years
      val fy1 = fyEndForAPStartDate.minusYears(1).getYear
      val fy2 = fyEndForAPStartDate.getYear

      val apdaysinfy1 = daysBetweenInclusive(accountingPeriodStart, fyEndForAPStartDate)
      val apdaysinfy2 = daysInAP - apdaysinfy1

      val maybeFY1Config = config.fyConfigs.find(_.year == fy1)
      val maybeFY2Config = config.fyConfigs.find(_.year == fy2)

      (maybeFY1Config, maybeFY2Config) match {
        case (Some(fy1Config), Some(fy2Config)) =>
          val apFY1Ratio = BigDecimal(apdaysinfy1) / daysInAP
          val apFY2Ratio = BigDecimal(apdaysinfy2) / daysInAP
          val fy1Ratio = fyRatioForAdjustingThresholds(
            maybeFY1Config.flatMap(_.upperThreshold),
            maybeFY2Config.flatMap(_.upperThreshold),
            apdaysinfy1,
            daysInFY(fy1),
            daysInAP
          )
          val fy2Ratio = fyRatioForAdjustingThresholds(
            maybeFY1Config.flatMap(_.upperThreshold),
            maybeFY2Config.flatMap(_.upperThreshold),
            apdaysinfy2,
            daysInFY(fy2),
            daysInAP
          )

          val accFY1 = calculateAssociatedComp(
            fy1Config,
            fy2Config,
            associatedCompanies,
            associatedCompaniesFY1,
            associatedCompaniesFY2,
            fy1Config.year
          )
          val accFY2 = calculateAssociatedComp(
            fy1Config,
            fy2Config,
            associatedCompanies,
            associatedCompaniesFY1,
            associatedCompaniesFY2,
            fy2Config.year
          )

          val CTWithAdjustments(
            adjustedProfitFY1,
            _,
            _,
            _,
            ctFY1
          ) =
            computeCorporationTax(
              profit,
              exemptDistributions,
              apFY1Ratio,
              fy1Config.lowerThreshold,
              Some(accFY1),
              fy1Ratio,
              fy1Config.smallProfitRate,
              fy1Config.mainRate
            )
          val CTWithAdjustments(
            adjustedProfitFY2,
            _,
            _,
            _,
            ctFY2
          ) =
            computeCorporationTax(
              profit,
              exemptDistributions,
              apFY2Ratio,
              fy2Config.lowerThreshold,
              Some(accFY2),
              fy2Ratio,
              fy2Config.smallProfitRate,
              fy2Config.mainRate
            )
          val mr1 = computeMarginalRelief(
            profit * apFY1Ratio,
            (profit + exemptDistributions) * apFY1Ratio,
            Some(accFY1),
            fy1Config.lowerThreshold,
            fy1Config.upperThreshold,
            fy1Ratio,
            fy1Config.marginalReliefFraction
          )
          val mr2 = computeMarginalRelief(
            profit * apFY2Ratio,
            (profit + exemptDistributions) * apFY2Ratio,
            Some(accFY2),
            fy2Config.lowerThreshold,
            fy2Config.upperThreshold,
            fy2Ratio,
            fy2Config.marginalReliefFraction
          )
          Right(
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
          )

        case (Some(_), None) =>
          Left(ConfigMissingError(one(fy2)))
        case (None, Some(_)) =>
          Left(ConfigMissingError(one(fy1)))
        case (None, None) =>
          Left(ConfigMissingError(NonEmptyList.of(fy1, fy2)))
      }
    }
  }

  def calculateAssociatedComp(
    fy1Config: FYConfig,
    fy2Config: FYConfig,
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

  case class CTWithAdjustments(
    adjustedProfit: BigDecimal,
    adjustedExemptDistributions: BigDecimal,
    adjustedAugmentedProfit: BigDecimal,
    adjustedLowerThreshold: BigDecimal,
    corporationTax: BigDecimal
  )

  private def computeCorporationTax(
    profit: BigDecimal,
    exemptDistributions: BigDecimal,
    apFYRatio: BigDecimal,
    maybeLowerThreshold: Option[Int],
    associatedCompanies: Option[Int],
    fyRatio: BigDecimal,
    smallProfitRate: Option[Double],
    mainRate: Double
  ): CTWithAdjustments = {

    val adjustedProfit = profit * apFYRatio
    val adjustedExemptDistributions = exemptDistributions * apFYRatio
    val adjustedAugmentedProfit = adjustedProfit + adjustedExemptDistributions

    maybeLowerThreshold match {
      case Some(lowerThreshold) =>
        val adjustedLT = BigDecimal(lowerThreshold) * fyRatio / BigDecimal(
          associatedCompanies.getOrElse(1)
        )
        // calculate corporation tax
        val corporationTax =
          adjustedProfit * (if (adjustedAugmentedProfit <= adjustedLT) BigDecimal(smallProfitRate.getOrElse(0.0))
                            else BigDecimal(mainRate))
        CTWithAdjustments(
          adjustedProfit,
          adjustedExemptDistributions,
          adjustedAugmentedProfit,
          adjustedLT,
          corporationTax
        )
      case None =>
        CTWithAdjustments(
          adjustedProfit,
          adjustedExemptDistributions,
          adjustedAugmentedProfit,
          BigDecimal(0),
          BigDecimal(mainRate) * adjustedProfit
        )
    }

  }

  private def computeMarginalRelief(
    adjustedProfit: BigDecimal,
    adjustedAugmentedProfit: BigDecimal,
    associatedCompanies: Option[Int],
    maybeLowerThreshold: Option[Int],
    maybeUpperThreshold: Option[Int],
    fyRatio: BigDecimal,
    marginalReliefFraction: Option[Double]
  ): BigDecimal =
    (maybeLowerThreshold, maybeUpperThreshold) match {
      case (Some(lowerThreshold), Some(upperThreshold)) =>
        // adjust upper and lower thresholds
        val adjustedLT = lowerThreshold * fyRatio / BigDecimal(
          associatedCompanies.getOrElse(1)
        )
        val adjustedUT = upperThreshold * fyRatio / BigDecimal(
          associatedCompanies.getOrElse(1)
        )
        // calculate marginal relief
        if (adjustedAugmentedProfit > adjustedLT && adjustedAugmentedProfit <= adjustedUT) {
          BigDecimal(
            marginalReliefFraction.getOrElse(1.0)
          ) * (adjustedUT - adjustedAugmentedProfit) * (adjustedProfit / adjustedAugmentedProfit)
        } else {
          BigDecimal(0)
        }
      case _ =>
        BigDecimal(0)
    }

  private def financialYearEnd(date: LocalDate): LocalDate =
    (if (date.getMonth.getValue >= Month.JANUARY.getValue && date.getMonth.getValue <= Month.MARCH.getValue) {
       date
     } else {
       date.plusYears(1)
     }).withMonth(Month.MARCH.getValue).withDayOfMonth(31)

  private def daysBetweenInclusive(start: LocalDate, end: LocalDate): Int =
    (start.until(end, ChronoUnit.DAYS) + 1).toInt

  private def daysInFY(year: Int): Int = {
    val start = LocalDate.of(year, 4, 1)
    daysBetweenInclusive(start, start.plusYears(1).withMonth(3).withDayOfMonth(31))
  }

  private def fyRatioForAdjustingThresholds(
    maybeUpperThresholdFY1: Option[Int],
    maybeUpperThresholdFY2: Option[Int],
    apDaysInFY: Int,
    fyDays: Int,
    daysInAP: Int
  ): BigDecimal =
    (maybeUpperThresholdFY1, maybeUpperThresholdFY2) match {
      case (Some(upperThresholdFY1), Some(upperThresholdFy2)) if upperThresholdFY1 != upperThresholdFy2 => // MR year
        BigDecimal(apDaysInFY) / fyDays
      case _ => // flat rate year
        BigDecimal(apDaysInFY) / (if (daysInAP == 366) 366 else 365)
    }

  private def roundUp(value: BigDecimal): Double =
    value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
