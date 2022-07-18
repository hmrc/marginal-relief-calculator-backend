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

package controllers

import cats.syntax.validated._
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import parameters.{ AskOnePart, AskParametersService, AssociatedCompaniesParameter, ConfigMissingError, Period }
import play.api.http.Status
import play.api.mvc.AnyContentAsEmpty
import play.api.test.Helpers.{ GET, contentAsJson, status, _ }
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.http.UnprocessableEntityException

import java.time.LocalDate

class AskParametersControllerSpec extends AnyFreeSpec with Matchers with IdiomaticMockito with ArgumentMatchersSugar {
  trait Fixture {
    val accountingPeriodStart: LocalDate = LocalDate.ofEpochDay(0)
    val accountingPeriodEnd: LocalDate = LocalDate.ofEpochDay(1)
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest(GET, "/params/associated-companies")
    val mockRequiredParametersService: AskParametersService = mock[AskParametersService]
    val controller = new AskParametersController(mockRequiredParametersService, Helpers.stubControllerComponents())
  }

  "GET /params/associated-companies" - {
    "return associated companies parameter info successfully" in new Fixture {
      mockRequiredParametersService.associatedCompaniesParameters(
        accountingPeriodStart,
        accountingPeriodEnd,
        0,
        None
      ) returns AskOnePart(Period(accountingPeriodStart, accountingPeriodEnd)).validNel

      val result = controller.associatedCompanies(accountingPeriodStart, accountingPeriodEnd, 0, None)(fakeRequest)

      status(result) shouldBe Status.OK
      contentAsJson(result).as[AssociatedCompaniesParameter] shouldBe AskOnePart(
        Period(accountingPeriodStart, accountingPeriodEnd)
      )
    }

    "returns error, when upstream service has error" in new Fixture {
      mockRequiredParametersService.associatedCompaniesParameters(
        accountingPeriodStart,
        accountingPeriodEnd,
        0,
        None
      ) returns ConfigMissingError(0).invalidNel

      try
        controller.associatedCompanies(accountingPeriodStart, accountingPeriodEnd, 0, None)(fakeRequest)
      catch {
        case e: Throwable =>
          e shouldBe a[UnprocessableEntityException]
          e.asInstanceOf[UnprocessableEntityException].getMessage shouldBe "Configuration missing for financial year: 0"
      }
    }
  }
}
