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

import julienrf.json.derived
import play.api.libs.json.{ Format, Json, OFormat, __ }

import java.time.LocalDate

sealed trait AssociatedCompaniesRequirement

object AssociatedCompaniesRequirement {
  implicit val format: OFormat[AssociatedCompaniesRequirement] =
    derived.flat.oformat[AssociatedCompaniesRequirement]((__ \ "type").format[String])
}

case class Period(start: LocalDate, end: LocalDate)
object Period {
  implicit val format: Format[Period] = Json.format[Period]
}

case object NotRequired extends AssociatedCompaniesRequirement
case class OnePeriod(period: Period) extends AssociatedCompaniesRequirement
case class TwoPeriods(period1: Period, period2: Period) extends AssociatedCompaniesRequirement
