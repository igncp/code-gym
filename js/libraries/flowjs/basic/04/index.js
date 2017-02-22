// @flow

// this is still work in progress

import fs from "fs"

import sampleJSON from "./sample.json"

import type {
  DotGraph,
  DotGraphType,
  DotStatement,
  DotStatementEdge,
  DotStatementNode,
  DotStatementMisc,
  JSONEdge,
  JSONGraph,
  JSONMisc,
  JSONNode,
} from "./types"

function getNodeStatementsFromNodes(nodes: JSONNode[]): DotStatementNode[] {
  return nodes.map((node: JSONNode): DotStatementNode => {
    return {
      id: `${node.id}`,
      type: "node",
    }
  })
}

function getEdgeStatementsFromEdges(edges: JSONEdge[]): DotStatementEdge[] {
  return edges.map((edge: JSONEdge): DotStatementEdge => {
    return {
      from: `${edge.from}`,
      to: `${edge.to}`,
      type: "edge",
    }
  })
}

function getMiscStatementsFromMisc(misc: JSONMisc[]): DotStatementMisc[] {
  return misc.map((m: JSONMisc): DotStatementMisc => {
    return {
      key: m.key,
      type: "misc",
      value: m.value,
    }
  })
}

function transformJsonGraphToDotGraph(g: JSONGraph): DotGraph {
  const nodeStatements: DotStatementNode[] = getNodeStatementsFromNodes(g.nodes)
  const edgeStatements: DotStatementEdge[] = getEdgeStatementsFromEdges(g.edges)
  const miscStatements: DotStatementMisc[] = getMiscStatementsFromMisc(g.misc)

  return {
    content: nodeStatements
      .concat(edgeStatements)
      .concat(miscStatements),
    type: "digraph",
  }
}

function wrapStrWithGraphType(str: string, dt: DotGraphType): string {
  return `${dt} { ${str} }`
}

function parseStatements(s: DotStatement[]): string {
  return s.reduce((): string => {
    return "foo"
  }, "")
}

function parseDotGraphToStr(dg: DotGraph): string {
  let result: string = ""

  result += parseStatements(dg.content)
  result += wrapStrWithGraphType(result, dg.type)

  return result
}

function writeDotGraphToFile(dg: DotGraph) {
  const content: string = parseDotGraphToStr(dg)

  fs.writeFileSync(`${__dirname}/output.dot`, content)
}

const jsonGraph: JSONGraph = sampleJSON
const dotGraph: DotGraph = transformJsonGraphToDotGraph(jsonGraph)

writeDotGraphToFile(dotGraph)
