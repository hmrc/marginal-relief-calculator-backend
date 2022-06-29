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
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.traverse._
import play.api.{ ConfigLoader, Configuration }

import scala.util.{ Failure, Success, Try }

object CalculatorConfigParser {

  type ValidationResult[A] = ValidatedNel[InvalidConfigError, A]

  def parse(config: Configuration): ValidationResult[List[FYConfig]] =
    config
      .get[Configuration]("calculator-config")
      .get[Seq[Configuration]]("fy-configs")
      .toList
      .traverse[ValidationResult, FYConfig] { configuration =>
        (
          validated[Int](configuration, "year"),
          optionalValidated[Int](configuration, "lower-threshold"),
          optionalValidated[Int](configuration, "upper-threshold"),
          optionalValidated[Double](configuration, "small-profit-rate"),
          validated[Double](configuration, "main-rate"),
          optionalValidated[Double](configuration, "marginal-relief-fraction")
        ).tupled
          .andThen {
            case (
                  year,
                  Some(lowerThreshold),
                  Some(upperThreshold),
                  Some(smallProfitRate),
                  mainRate,
                  Some(marginalReliefFraction)
                ) =>
              MarginalReliefConfig(
                year,
                lowerThreshold,
                upperThreshold,
                smallProfitRate,
                mainRate,
                marginalReliefFraction
              ).validNel
            case (year, None, None, None, mainRate, None) =>
              FlatRateConfig(year, mainRate).validNel
            case (year, _, _, _, _, _) =>
              InvalidConfigError(
                s"Invalid config for year $year. For flat rate year, you need to specify year and main-rate only. " +
                  s"For marginal relief year, you need to specify year, lower-threshold, upper-threshold, small-profit-rate, " +
                  s"main-rate and marginal-relief-fraction"
              ).invalidNel
          }
      }
      .andThen { fyConfigs =>
        val years = fyConfigs.map(_.year)
        val duplicates = years.diff(years.distinct).distinct
        if (duplicates.nonEmpty) {
          InvalidConfigError(s"Duplicate config found for years ${duplicates.mkString(",")}").invalidNel
        } else {
          fyConfigs.validNel
        }
      }

  private def optionalValidated[T](configuration: Configuration, key: String)(implicit
    loader: ConfigLoader[T]
  ): ValidationResult[Option[T]] =
    Try(configuration.getOptional[T](key)) match {
      case Success(maybeValue) => maybeValue.validNel
      case Failure(_)          => InvalidConfigError(s"$key is invalid").invalidNel
    }

  private def validated[T](configuration: Configuration, key: String)(implicit
    loader: ConfigLoader[T]
  ): ValidationResult[T] =
    Try(configuration.get[T](key)) match {
      case Success(value) => value.validNel
      case Failure(_)     => InvalidConfigError(s"$key is missing or invalid").invalidNel
    }
}
