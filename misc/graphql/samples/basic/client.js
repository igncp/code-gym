const uri = "http://localhost:4000/graphql";
const gql = require("graphql-tag");
const fetch = require("node-fetch");
const {
  ApolloClient,
  HttpLink,
  InMemoryCache,
} = require("@apollo/client/core");

const initialQuery = `{ hello }`;
const query = gql(initialQuery);

console.log('client.js: query', query);

const main = async () => {
  const apolloClient = new ApolloClient({
    link: new HttpLink({
      uri,
      fetch,
    }),
    cache: new InMemoryCache(),
  });

  const { data: result } = await apolloClient.query({
    query,
  });

  console.log("client.js: result", JSON.stringify(result, null, 2));
};

main();
