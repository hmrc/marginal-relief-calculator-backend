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

import calculator.{ ConfigMissingError, MarginalReliefCalculator, MarginalReliefResult, SingleResult }
import cats.syntax.validated._
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.Status
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.{ FakeRequest, Helpers }
import uk.gov.hmrc.http.UnprocessableEntityException

import java.time.LocalDate
import scala.concurrent.Future

class MarginalReliefCalculatorControllerSpec
    extends AnyWordSpec with Matchers with IdiomaticMockito with ArgumentMatchersSugar {

  trait Fixture {
    val accountingPeriodStart = LocalDate.ofEpochDay(0)
    val accountingPeriodEnd = LocalDate.ofEpochDay(1)
    val fakeRequest = FakeRequest(GET, "/calculate")
    val mockCalculator = mock[MarginalReliefCalculator]
    val controller = new MarginalReliefCalculatorController(Helpers.stubControllerComponents(), mockCalculator)
  }

  "GET /calculate" should {

    "return calculator result successfully" in new Fixture {

      mockCalculator.compute(accountingPeriodStart, accountingPeriodEnd, 0, 0, None, None, None) returns
        SingleResult(1970, 0, 0, 0, 0, 0).validNel

      val result: Future[Result] =
        controller.calculate(accountingPeriodStart, accountingPeriodEnd, 0, None, None, None, None)(fakeRequest)
      status(result) shouldBe Status.OK
      contentAsJson(result).as[MarginalReliefResult] shouldBe SingleResult(1970, 0, 0, 0, 0, 0)
    }

    "throw error when config missing for financial year" in new Fixture {

      mockCalculator.compute(accountingPeriodStart, accountingPeriodEnd, 0, 0, None, None, None) returns
        ConfigMissingError(0).invalidNel

      try
        controller.calculate(accountingPeriodStart, accountingPeriodEnd, 0, None, None, None, None)(fakeRequest)
      catch {
        case e: Throwable =>
          e shouldBe a[UnprocessableEntityException]
          e.asInstanceOf[UnprocessableEntityException].getMessage shouldBe "Configuration missing for financial year: 0"
      }
    }
  }
}
