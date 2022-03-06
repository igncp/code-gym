import "./App.css";
import { client } from "./utils/graphQLClient";
import ExchangeRatePage from "./ExchangeRatePage";

import { ApolloProvider } from "@apollo/react-hooks";

function App() {
  return (
    <ApolloProvider client={client}>
      <ExchangeRatePage />
    </ApolloProvider>
  );
}

export default App;
