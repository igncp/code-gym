# Official Docs

- https://graphql.org

- [ ] [FAQ](https://graphql.org/faq)
    - [x] Getting Started
    - [x] General
    - [ ] Best Practices
    - [ ] ...

- [ ] [Main tutorial GraphQL](https://graphql.org/)
    - [x] [Introduction](https://graphql.org/learn/)
        - GraphQL is a query language and a server-side runtime
        - A schema is defined by types and fields
    - [x] [Queries and Mutations](https://graphql.org/learn/queries/)
        - In a query, a field can accept arguments
        - Can rename response fields with aliases
        - Fragments are collections of fields, which can be reused inside queries
            - Fragments can access variables defined in the query or mutation using them
        - When writing `query Foo { ...  }`, the `query` keyword is the "operation type", and `Foo` is the "operantion name"
            - The "operation types" are: `query`, `mutation` and `subscription`
            - Adding the "operation name" is only required on "multi-operation" documents, although it is encouraged
        - All declared variables must be scalars, enums, or input object types
            - Variables can have default values
        - The core GraphQL specification includes two directives: `@include` and `@skip`
        - By convention, any request that changes data in the server is done via `mutation` (similar to POST in REST)
        - An important distinction between `query` and `mutation` operations is:
            - Fields in `query` execute in parallel in the server, but `mutation` fields execute in series
        - In GraphQL there are interfaces and union types
        - GraphQL has a meta field called `__typename` which returns a string
    - [x] [Schemas and Types](https://graphql.org/learn/schema/)
        - In the schema, fields ending in a exclamation mark indicate that they are non-nullable
        - The scalar types which are in the specification: `Int`, `Float`, `String`, `Boolean`, `ID`
        - GraphQL supports defining custom scalar types
        - It also supports defining interfaces: when defining types, they can implement them, for example:
            - `type Droid implements Character`
        - There is also input types, which can be used for variables
    - [ ] [Validation](https://graphql.org/learn/validation/)
    - [ ] ...
