var neo4j = require('neo4j');
var Promise = require("bluebird");
var db = new neo4j.GraphDatabase('http://neo4j:neo4j@localhost:7474');

Promise
  .resolve({})
  .then(clearDatabase)
  .then(createNodes)
  .then(getFooNode)
  .then(logResult)


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
      query: "CREATE (n:Person { foo : 'bar' }), (m:Person { foo : 'baz' })"
    }, function(err, results) {
      if (err) throw err;
      resolve()
    })
  })
}

function getFooNode() {
  return new Promise(function(resolve, reject) {
    db.cypher({
      query: 'MATCH (u:Person {foo: {foo}}) RETURN u',
      params: {
        foo: 'bar',
      },
    }, function(err, results) {
      if (err) throw err;
      resolve(results)
    })
  })
}

function logResult(results) {
  if (results.length === 0) {
    console.log('No results found.');
  } else {
    console.log('results: ', results.length)
    var result = results[0]
    var user = result['u'];
    console.log("result: ", JSON.stringify(user, null, 4));
  }
}