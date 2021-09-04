const elasticsearch = require("elasticsearch");
const esb = require("elastic-builder");

const client = new elasticsearch.Client({
  node: "http://localhost:9200",
  log: "trace",
  httpAuth: "elastic:changeme",
});

const main = async () => {
  try {
    const body = esb
      .requestBodySearch()
      .query(
        esb
          .boolQuery()
          .must([
            esb.rangeQuery("Year of Release").gte(2000),
            esb.matchQuery("Award", "Winner"),
          ])
      )
      .toJSON();

    const resp = await client.search({
      body,
      index: "demo-csv",
    });

    console.log("query-csv.js: resp.hits.hits", resp.hits.total.value);
  } catch (err) {
    console.error("ERROR", err.message);
  }
};

main();
