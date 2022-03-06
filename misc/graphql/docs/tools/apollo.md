# Apollo

## Server

- Docs: https://www.apollographql.com/docs/apollo-server/

## Misc

- In previous versions of the library, a link had to manually configured. Currently it is not necessary for a conventional REST usage
    - Docs on link: https://www.apollographql.com/docs/react/api/link/introduction/

## Client

- https://github.com/apollographql/apollo-client
- JS docs: https://www.apollographql.com/docs/react/
    - This is a common tool used to parse queries: https://github.com/apollographql/graphql-tag
- There is a CLI tool: https://www.apollographql.com/docs/rover/
- In the docs it describes the client as a state management library (not a fetch management library)
    - So it is a replacement for redux
    - It has the concept of "field policies" and "reactive variables"

### Integration with React

- Need to use a `Provider` to pass the client down the context
- There are several hooks available `useQuery`, and `useLazyQuery` for more configuration
- The argument is `uri` (not `url`)

### [ ] Official Guide: React

- https://www.apollographql.com/docs/react
- Currently: Queries
- Can pass variables to the `useQuery` hooks in the second argument
- To sync existing data with the server, there are two strategies: polling and refetching
- To change the error handling, there is a `errorPolicy` option that can be passed to the hook
    - There are other policies options like `fetchPolicy` and `nextFetchPolicy`
- Currently: Mutations

## Future readings

- [ ] Redux migration to Apollo: https://www.apollographql.com/blog/apollo-client/architecture/redux-to-apollo-data-access-patterns/
- [ ] How to GraphQL with react and apollo introduction: : https://www.howtographql.com/react-apollo/0-introduction/
