// @flow

export type JSONNodeID = number
export type JSONLabel = string
export type JSONNode = {
  id: JSONNodeID,
  label: JSONLabel
}
export type JSONEdge = {
  from: JSONNodeID,
  label: JSONLabel,
  to: JSONNodeID,
}
export type JSONMisc = {
  key: string,
  value: string,
}
export type JSONGraph = {
  edges: JSONEdge[],
  misc: JSONMisc[],
  nodes: JSONNode[],
}

export type StatementTypeNode = "node"
export type StatementTypeEdge = "edge"
export type StatementTypeMisc = "misc"
export type DotStatementNodeId = string
export type DotStatementEdgeFrom = DotStatementNodeId
export type DotStatementEdgeTo = DotStatementNodeId
export type DotStatementMisc = {|
  key: string,
  type: StatementTypeMisc,
  value: string,
|}
export type DotStatementNode = {|
  id: DotStatementNodeId,
  type: StatementTypeNode,
|}
export type DotStatementEdge = {|
  from: DotStatementEdgeFrom,
  to: DotStatementEdgeTo,
  type: StatementTypeEdge,
|}

export type DotGraphType = "digraph" | "graph"
export type DotStatement = DotStatementNode | DotStatementEdge | DotStatementMisc
export type DotGraphContent = DotStatement[]
export type DotGraph = {
  content: DotGraphContent,
  type: DotGraphType,
}
