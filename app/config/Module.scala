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

import calculator.{ CalculatorConfig, FYConfig, MarginalReliefCalculator, MarginalReliefCalculatorImpl }
import com.google.inject.AbstractModule

class Module extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[AppConfig]).asEagerSingleton()
    bind(classOf[MarginalReliefCalculator]).to(classOf[MarginalReliefCalculatorImpl]).asEagerSingleton()
    bind(classOf[CalculatorConfig]).toInstance(
      CalculatorConfig(Seq(FYConfig(2022, 0, 0, 0, 0.19, None), FYConfig(2023, 50000, 250000, 0.19, 0.25, Some(0.015))))
    )
  }
}
