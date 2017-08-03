global.__TEST__ = true

const esprima = require("esprima")

const script = require("./script")

describe("script", () => {
  describe("getPathFromVariableDeclaration", () => {
    test("", () => {
      const ast = esprima.parse("const {foo} = bar.baz")
      const vd = ast.body[0]
      const result = script._test.getPathsFromVariableDeclaration(vd)

      expect(result).toEqual(["bar.baz.foo"])
    })

    test("", () => {
      const ast = esprima.parse("const {foo} = bar")
      const vd = ast.body[0]
      const result = script._test.getPathsFromVariableDeclaration(vd)

      expect(result).toEqual(["bar.foo"])
    })

    test("", () => {
      const ast = esprima.parse("const {foo, bar} = baz")
      const vd = ast.body[0]
      const result = script._test.getPathsFromVariableDeclaration(vd)

      expect(result).toEqual(["baz.foo", "baz.bar"])
    })

    test("", () => {
      const ast = esprima.parse("const {fourA, fourB} = one.two.three")
      const vd = ast.body[0]
      const result = script._test.getPathsFromVariableDeclaration(vd)

      expect(result).toEqual(["one.two.three.fourA", "one.two.three.fourB"])
    })
  })
})
