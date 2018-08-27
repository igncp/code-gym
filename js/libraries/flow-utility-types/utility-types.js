// @flow

type T_Foo = {
  foo: {
    bar: string
  }
}

type T_extractBar = <T>(T) => $PropertyType<$PropertyType<T, 'foo'>, 'bar'>

type T_Bar = $Call<T_extractBar, T_Foo>

const bar: T_Bar = "bar" // eslint-disable-line no-unused-vars
// const bar2: T_Bar = 1 // fails
