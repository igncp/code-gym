import { ApolloClient, InMemoryCache } from "@apollo/client";

// https://www.apollographql.com/docs/react/api/core/ApolloClient/
export const client = new ApolloClient({
  cache: new InMemoryCache(),
  uri: "https://48p1r2roz4.sse.codesandbox.io",
});
