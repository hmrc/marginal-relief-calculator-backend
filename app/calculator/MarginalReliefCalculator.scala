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
import com.google.inject.{ Inject, Singleton }

import java.time.temporal.ChronoUnit
import java.time.{ LocalDate, Month }

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
class MarginalReliefCalculatorImpl @Inject() (config: CalculatorConfig) extends MarginalReliefCalculator {

  override def compute(
    accountingPeriodStart: LocalDate,
    accountingPeriodEnd: LocalDate,
    profit: BigDecimal,
    exemptDistributions: BigDecimal,
    associatedCompanies: Option[Int],
    associatedCompaniesFY1: Option[Int],
    associatedCompaniesFY2: Option[Int]
  ): Either[CalculatorError, MarginalReliefResult] = {

    val daysInAP = daysBetweenInclusive(accountingPeriodStart, accountingPeriodEnd)
    val fyEndForAPStartDate = financialYearEnd(accountingPeriodStart)

    if (
      fyEndForAPStartDate
        .isEqual(accountingPeriodEnd) || fyEndForAPStartDate.isAfter(accountingPeriodEnd)
    ) {
      // one financial year
      val maybeFYConfig = config.fyConfigs.find(_.year == accountingPeriodStart.getYear)

      maybeFYConfig match {
        case Some(fyConfig) =>
          val fyDays = financialYearDays(daysInAP)
          val fyRatio = BigDecimal(daysInAP) / fyDays

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
          if (thresholdsUnchanged(fy1Config, fy2Config) && ratesUnchanged(fy1Config, fy2Config)) {
            // 1. thresholds and rates unchanged - 1 period
            val fyDays = financialYearDays(daysInAP)
            val fyRatio = BigDecimal(daysInAP) / fyDays
            val CTWithAdjustments(
              _,
              _,
              _,
              _,
              corporationTaxBeforeMR
            ) = computeCorporationTax(
              profit,
              exemptDistributions,
              1,
              fy1Config.lowerThreshold,
              associatedCompanies.map(_ + 1),
              fyRatio,
              fy1Config.smallProfitRate,
              fy1Config.mainRate
            )
            val marginalRelief = computeMarginalRelief(
              profit,
              profit + exemptDistributions,
              associatedCompanies.map(_ + 1),
              fy1Config.lowerThreshold,
              fy1Config.upperThreshold,
              fyRatio,
              fy1Config.marginalReliefFraction
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
          } else if (thresholdsUnchanged(fy1Config, fy2Config) && !ratesUnchanged(fy1Config, fy2Config)) {
            // 2. thresholds unchanged, rates change - profits needs to apportioned, rates applied - 1 period
            val fyDays = financialYearDays(daysInAP)
            val fyRatio = BigDecimal(daysInAP) / fyDays
            val apfy1ratio = BigDecimal(apdaysinfy1) / daysInAP
            val apfy2ratio = BigDecimal(apdaysinfy2) / daysInAP
            val CTWithAdjustments(
              _,
              _,
              _,
              _,
              ctFY1
            ) =
              computeCorporationTax(
                profit,
                exemptDistributions,
                apfy1ratio,
                fy1Config.lowerThreshold,
                associatedCompanies.map(_ + 1),
                fyRatio,
                fy1Config.smallProfitRate,
                fy1Config.mainRate
              )
            val CTWithAdjustments(
              _,
              _,
              _,
              _,
              ctFY2
            ) =
              computeCorporationTax(
                profit,
                exemptDistributions,
                apfy2ratio,
                fy2Config.lowerThreshold,
                associatedCompanies.map(_ + 1),
                fyRatio,
                fy2Config.smallProfitRate,
                fy2Config.mainRate
              )
            val marginalRelief = computeMarginalRelief(
              profit,
              profit + exemptDistributions,
              associatedCompanies.map(_ + 1),
              fy1Config.lowerThreshold,
              fy1Config.upperThreshold,
              fyRatio,
              fy1Config.marginalReliefFraction
            )

            val corporationTaxBeforeMR = ctFY1 + ctFY2
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
          } else {
            // 3. thresholds change, rates changed/unchanged - thresholds needs to be apportioned, profits need to be apportioned - 2 periods
            val apFY1Ratio = BigDecimal(apdaysinfy1) / daysInAP
            val apFY2Ratio = BigDecimal(apdaysinfy2) / daysInAP
            val fyDays = financialYearDays(daysInAP)
            val fy1Ratio = BigDecimal(apdaysinfy1) / fyDays
            val fy2Ratio = BigDecimal(apdaysinfy2) / fyDays

            val accFY1 = calculateAssociatedComp(
              fy1Config,
              fy2Config,
              associatedCompaniesFY1,
              associatedCompaniesFY2,
              (acc1, _) => acc1
            )
            val accFY2 = calculateAssociatedComp(
              fy1Config,
              fy2Config,
              associatedCompaniesFY1,
              associatedCompaniesFY2,
              (_, acc2) => acc2
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
          }
        case (Some(_), None) =>
          Left(ConfigMissingError(one(fy2)))
        case (None, Some(_)) =>
          Left(ConfigMissingError(one(fy1)))
        case (None, None) =>
          Left(ConfigMissingError(NonEmptyList.of(fy1, fy2)))
      }
    }
  }

  private def roundUp(value: BigDecimal): Double =
    value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  private def calculateAssociatedComp(
    fy1Config: FYConfig,
    fy2Config: FYConfig,
    associatedCompaniesFY1: Option[Int],
    associatedCompaniesFY2: Option[Int],
    default: (Int, Int) => Int
  ): Int =
//    val lrma1 = fy1Config.lowerThreshold
//    val urma1 = fy1Config.upperThreshold
//    val lrma2 = fy2Config.lowerThreshold
//    val urma2 = fy2Config.upperThreshold
//    if (lrma1 == lrma2 && urma1 == urma2) { // TODO: is this logic ever used?
//      Math.max(associatedCompaniesFY1.getOrElse(0), associatedCompaniesFY2.getOrElse(0)) + 1
//    } else {
    default(associatedCompaniesFY1.getOrElse(0), associatedCompaniesFY2.getOrElse(0)) + 1
//    }

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
    lowerThreshold: Int,
    associatedCompanies: Option[Int],
    fyRatio: BigDecimal,
    smallProfitRate: Double,
    mainRate: Double
  ): CTWithAdjustments = {
    val adjustedProfit = profit * apFYRatio
    val adjustedExemptDistributions = exemptDistributions * apFYRatio
    val adjustedAugmentedProfit = adjustedProfit + adjustedExemptDistributions
    val adjustedLowerThreshold = BigDecimal(lowerThreshold) * fyRatio / BigDecimal(
      associatedCompanies.getOrElse(1)
    )
    // calculate corporation tax
    val corporationTax = if (adjustedLowerThreshold == BigDecimal(0)) { // threshold 0 means its a flat rate year
      BigDecimal(mainRate) * adjustedProfit
    } else if (adjustedAugmentedProfit <= adjustedLowerThreshold) {
      BigDecimal(smallProfitRate) * adjustedProfit
    } else {
      BigDecimal(mainRate) * adjustedProfit
    }
    CTWithAdjustments(
      adjustedProfit,
      adjustedExemptDistributions,
      adjustedAugmentedProfit,
      adjustedLowerThreshold,
      corporationTax
    )
  }

  private def computeMarginalRelief(
    adjustedProfit: BigDecimal,
    adjustedAugmentedProfit: BigDecimal,
    associatedCompanies: Option[Int],
    lowerThreshold: BigDecimal,
    upperThreshold: BigDecimal,
    fyRatio: BigDecimal,
    marginalReliefFraction: Option[Double]
  ): BigDecimal =
    if (lowerThreshold == BigDecimal(0)) { // threshold 0 means its a flat rate year, so no marginal relief
      BigDecimal(0)
    } else {
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
    }

  private def thresholdsUnchanged(
    fy1Config: FYConfig,
    fy2Config: FYConfig
  ): Boolean =
    fy1Config.lowerThreshold == fy2Config.lowerThreshold && fy1Config.upperThreshold == fy2Config.upperThreshold

  private def ratesUnchanged(
    fy1Config: FYConfig,
    fy2Config: FYConfig
  ): Boolean =
    fy1Config.smallProfitRate == fy2Config.smallProfitRate && fy1Config.mainRate == fy2Config.mainRate

  private def financialYearEnd(date: LocalDate): LocalDate =
    (if (date.getMonth.getValue >= Month.JANUARY.getValue && date.getMonth.getValue <= Month.MARCH.getValue) {
       date
     } else {
       date.plusYears(1)
     }).withMonth(Month.MARCH.getValue).withDayOfMonth(31)

  private def daysBetweenInclusive(start: LocalDate, end: LocalDate): Long =
    start.until(end, ChronoUnit.DAYS) + 1

  private def financialYearDays(daysInAP: Long): Long =
    if (daysInAP == 366) {
      366
    } else {
      365
    }
}
