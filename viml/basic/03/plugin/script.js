const {
  readFileSync,
  writeFileSync,
} = require("fs")

const {parse} = require("esprima")
const {generate} = require("escodegen")

const getPathFromMemberExpression = (me, acc = "") => {
  const prop = me.property.name

  if (me.object.type === "MemberExpression") {
    return getPathFromMemberExpression(me.object, `${prop}.${acc}`)
  }

  return `${me.object.name}.${prop}.${acc}`
}

const getPathsFromVariableDeclaration = (vd) => {
  const keys = vd.declarations[0].id.properties.map((p) => p.key.name)
  const declaration = vd.declarations[0]
  const path = (() => {
    if (declaration.init.type === "MemberExpression") {
      return getPathFromMemberExpression(declaration.init)
    } else if (declaration.init.type === "Identifier") {
      return `${declaration.init.name}.`
    }

    return ""
  })()

  return keys.map((key) => `${path}${key}`)
}

const main = () => {
  const fileContent = readFileSync("/tmp/log", "utf8")
  const ast = parse(fileContent)

  const block = ast.body[0].declarations[0].init.body

  block.body.forEach((blockItem) => {
    if (blockItem.type === "VariableDeclaration") {
      const path = getPathsFromVariableDeclaration(blockItem).join("\n")

      if (!global.__TEST__) {
        console.log("path", path)
      }
    }
  })

  const result = generate(ast)

  const content = JSON.stringify(block, null, 4)

  writeFileSync("/tmp/log-2", content)

  console.log(result)
}

if (global.__TEST__) {
  module.exports._test = {
    getPathsFromVariableDeclaration,
  }
} else {
  main()
}
