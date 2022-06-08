package it

import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.ws.WSClient

trait BaseITSpec { this: GuiceOneServerPerSuite =>

  protected val wsClient: WSClient = app.injector.instanceOf[WSClient]
  protected val baseUrl: String = s"http://localhost:$port/marginal-relief-calculator-backend"

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure("metrics.enabled" -> false, "auditing.enabled" -> false)
      .build()
}
