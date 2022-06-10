package it

import org.scalatest.concurrent.{ IntegrationPatience, ScalaFutures }
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import uk.gov.hmrc.play.bootstrap.backend.http.ErrorResponse

class MarginalReliefCalculatorEndpointITSpec
    extends AnyWordSpec with Matchers with ScalaFutures with IntegrationPatience with GuiceOneServerPerSuite
    with BaseITSpec {

  private val calculateUrl = s"$baseUrl/calculate"

  "calculate endpoint" should {
    "return calculation result when given parameters are valid" in {
      val response =
        wsClient
          .url(calculateUrl)
          .withQueryStringParameters(
            ("accountingPeriodStart", "2023-04-01"),
            ("accountingPeriodEnd", "2024-03-31"),
            ("profit", "60000")
          )
          .get()
          .futureValue
      response.status shouldBe 200
      response.body shouldBe """{"type":"SingleResult","effectiveTaxRateBeforeMR":25,"corporationTaxBeforeMR":15000,"effectiveTaxRate":20.25,"marginalRelief":2850,"corporationTax":12150}""".stripMargin
    }

    "return bad request error when required parameters are missing (profit missing)" in {
      val response =
        wsClient
          .url(calculateUrl)
          .withQueryStringParameters(("accountingPeriodStart", "2023-04-01"), ("accountingPeriodEnd", "2024-03-31"))
          .get()
          .futureValue
      response.status shouldBe 400
      response.body shouldBe """{"statusCode":400,"message":"Missing parameter: profit"}"""
    }

    "return bad request error when parameter value is in invalid format (accountingPeriodStart date)" in {
      val response =
        wsClient
          .url(calculateUrl)
          .withQueryStringParameters(
            ("accountingPeriodStart", "invalid"),
            ("accountingPeriodEnd", "2024-03-31"),
            ("profit", "60000")
          )
          .get()
          .futureValue
      response.status shouldBe 400
      response.json.as[ErrorResponse] shouldBe ErrorResponse(
        400,
        "Cannot parse parameter accountingPeriodStart as Date: For input string: \"REDACTED\""
      )
    }

    "return unprocessable entity error when accounting period financial year is not configured" in {
      val response =
        wsClient
          .url(calculateUrl)
          .withQueryStringParameters(
            ("accountingPeriodStart", "2020-04-01"),
            ("accountingPeriodEnd", "2021-03-31"),
            ("profit", "60000")
          )
          .get()
          .futureValue
      response.status shouldBe 422
      response.json.as[ErrorResponse] shouldBe ErrorResponse(422, "Configuration missing for financial year(s): 2020")
    }
  }
}
