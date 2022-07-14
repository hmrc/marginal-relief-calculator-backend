# Marginal Relief Calculator

This service provides an API for calculating the marginal relief and corporation tax liablity, based on the available configuration for financial years. It
also provides a break-down of the calculation to interpreting how the marginal relief and CT liablity was calculated.

## Running in DEV mode

To start the service locally, execute the following command

```$ sbt -jvm-debug DEBUG_PORT run -Dapplication.router=testOnlyDoNotUseInAppConf.Routes ```

To run locally using Service Manager

```sm --start MARGINAL_RELIEF_CALCULATOR_BACKEND```

## REST API Details

### Calculate

Calculates the marginal relief, based on the financial year config and the user parameters.

**Method:** `GET`

**Path:** `/marginal-relief-calculator-backend/calculate`

**Query Params**

|Name|Type|Description|Required|Format|Example Value|
|----|----|-----------|--------|------|-------------|
|accountingPeriodStart|Date|The accounting period start date|Yes|YYYY-MM-DD|2023-01-01|
|accountingPeriodEnd|Date|The accounting period end date|Yes|YYYY-MM-DD|2023-01-01|
|profit|Integer|The total taxable profit|Yes||100000|
|exemptDistributions|Integer|Exempt Distributions|No||10000|
|associatedCompanies|Integer|Number of associated companies|No||1|
|associatedCompaniesFY1|Integer|Number of associated companies for financial year 1, when accounting period spans 2 financial years|No||1|
|associatedCompaniesFY2|Integer|Number of associated companies for financial year 2, when accounting period spans 2 financial years|No||1|

**Responses**

|Status|Code|Response Body|Field Path|Field Message|
|------|----|-------------|----------|-------------|
|200| OK| Marginal relief caculation result as JSON| | |

When successful, the result can either be calculations for a single year or two years (when accounting period spans multiple years and there is change in rates/thresholds)

*Single Result*

```json
 {
     "type": "SingleResult",
     "year": 2022,
     "effectiveTaxRateBeforeMR": 25,
     "corporationTaxBeforeMR":15000,
     "effectiveTaxRate":20.25,
     "marginalRelief":2850,
     "corporationTax":12150
 }
 ```
 *Dual Result*

 ```json
  {
      "type": "DualResult",
      "year1": {
        "year": 2022,
        "corporationTaxBeforeMR": 10000,
        "effectiveTaxRateBeforeMR": 19,
        "corporationTax": 10000,
        "effectiveTaxRate": 10000,
        "marginalRelief": 0
      },
      "year2": {
        "year": 2023,
        "corporationTaxBeforeMR": 10000,
        "effectiveTaxRateBeforeMR": 25,
        "corporationTax": 8000,
        "effectiveTaxRate": 22,
        "marginalRelief": 2000
      },
      "effectiveTaxRateBeforeMR": 23,
      "effectiveTaxRate": 22
  }
  ```

### Required Parameters - Associated Companies

Returns the associated companies parameter requirements, given the accounting period, profit and exempt distributions

**Method:** `GET`

**Path:** `/marginal-relief-calculator-backend/ask-params/associated-companies`

**Query Params**

|Name|Type|Description|Required|Format|Example Value|
|----|----|-----------|--------|------|-------------|
|accountingPeriodStart|Date|The accounting period start date|Yes|YYYY-MM-DD|2023-01-01|
|accountingPeriodEnd|Date|The accounting period end date|Yes|YYYY-MM-DD|2023-01-01|
|profit|Integer|The total taxable profit|Yes||100000|
|exemptDistributions|Integer|Exempt Distributions|No||10000|

**Responses**

|Status|Code|Response Body|Field Path|Field Message|
|------|----|-------------|----------|-------------|
|200| OK| AssociatedCompaniesRequirement as JSON| | |

When successful, the result can either be NotRequired, OnePeriod or TwoPeriod results. When the requirement is for one period (OnePeriod type), the calculate request expects the associated companies
value via the associatedCompanies query param. When the requirement is for two notional periods (TwoPeriod type), the calculate request expected associated companies via associatedCompaniesFY1
and associatedCompaniesFY2 parameters. Note that one period may be retuned when the accounting period falls in a single financial year or one of the notional accounting periods falls under
Marginal Relief year.

*Not Required*

```json
  {
    "type": "NotRequired"
  }
```

*OnePeriod Result*

```json
 {
     "type": "OnePeriod",
     "period": {
        "start": "2020-01-01",
        "end": "2020-12-31"
     }
 }
 ```
 *TwoPeriod Result*

 ```json
  {
      "type": "TwoPeriod",
      "period1": {
         "start": "2020-01-01",
         "end": "2020-03-31"
      },
      "period2": {
         "start": "2020-04-01",
         "end": "2020-12-31"
      }
  }
  ```

