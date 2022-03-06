import React from "react";
import { useQuery, gql } from "@apollo/client";

const EXCHANGE_RATES = gql`
  query GetExchangeRates {
    rates(currency: "AUD") {
      currency
      rate
    }
  }
`;

function ExchangeRatePage() {
  const { data, loading, error } = useQuery(EXCHANGE_RATES);
  console.log("ExchangeRatePage.js: loading", loading);
  console.log("ExchangeRatePage.js: data", data);
  console.log("ExchangeRatePage.js: error", error);

  if (loading) {
    return <div>loading</div>;
  }

  if (error) {
    return <div>{error}</div>;
  }

  return data.rates.map(({ currency, rate }) => (
    <div key={currency}>
      <p>
        {currency}: {rate}
      </p>
    </div>
  ));
}

export default ExchangeRatePage;
