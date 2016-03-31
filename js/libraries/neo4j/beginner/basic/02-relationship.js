var neo4j = require('neo4j');
var Promise = require("bluebird");
var db = new neo4j.GraphDatabase('http://neo4j:neo4j@localhost:7474');

Promise
  .resolve({})
  .then(clearDatabase)
  .then(createNodes)
  .then(createRelationship)


function clearDatabase() {
  return new Promise(function(resolve, reject) {
    db.cypher({
      query: "MATCH (n) DETACH DELETE n"
    }, function(err, results) {
      if (err) throw err;
      resolve()
    })
  })
}

function createNodes() {
  return new Promise(function(resolve, reject) {
    db.cypher({
      query: "CREATE (n:A { foo : 'bar' }), (m:B { foo : 'baz' })"
    }, function(err, results) {
      if (err) throw err;
      resolve()
    })
  })
}

function createRelationship() {
  return new Promise(function(resolve, reject) {
    db.cypher({
      query: 'MATCH (n:A),(m:B) CREATE (n)-[r:foos {some: ["data", "in", "array"]}]->(m)',
    }, function(err, results) {
      if (err) throw err;
      resolve(results)
    })
  })
}
