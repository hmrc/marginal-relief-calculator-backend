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

import julienrf.json.derived
import play.api.libs.json.{ Format, Json, OFormat, __ }

case class MarginalReliefByYear(
  year: Int,
  corporationTaxBeforeMR: Double,
  effectiveTaxRateBeforeMR: Double,
  corporationTax: Double,
  effectiveTaxRate: Double,
  marginalRelief: Double
)
object MarginalReliefByYear {
  implicit val format: Format[MarginalReliefByYear] = Json.format[MarginalReliefByYear]
}

sealed trait MarginalReliefResult
object MarginalReliefResult {
  implicit val format: OFormat[MarginalReliefResult] =
    derived.flat.oformat[MarginalReliefResult]((__ \ "type").format[String])
}

case class SingleResult(
  year: Int,
  corporationTaxBeforeMR: Double,
  effectiveTaxRateBeforeMR: Double,
  corporationTax: Double,
  effectiveTaxRate: Double,
  marginalRelief: Double
) extends MarginalReliefResult
case class DualResult(
  year1: MarginalReliefByYear,
  year2: MarginalReliefByYear,
  effectiveTaxRateBeforeMR: Double,
  effectiveTaxRate: Double
) extends MarginalReliefResult
