Some notes and scripts to get more experience with the ELK stack

- Setup:
    - https://github.com/deviantony/docker-elk

- Kibana: `http://localhost:5601/`
- Logstash TCP: `http://localhost:5000/`
- Logstash Monitoring API: `http://localhost:9600/`
- Elasticsearch API: `http://localhost:9200/`

## Notes

- Default user: `elastic` / `changeme`
- For injecting data: `sudo pacman -S gnu-netcat` (installs `nc`)
    - You can also import files manually
- Convert string to number in Logstash: https://discuss.elastic.co/t/logstash-convert-string-to-integer/96325
- Pipelines Management GUI: `Kibana > Management > Ingest Node Pipelines`
- The most relevant configuration files (excluding the `extensions` directory):
    - `docker-elk/docker-compose.yml`
    - `docker-elk/elasticsearch/config/elasticsearch.yml`: uses [xpack][1], for example has the trial/basic plan
    - `docker-elk/kibana/config/kibana.yml`: connection to elastic container using [xpack][1]
    - `docker-elk/logstash/config/logstash.yml`: connection to elastic container using [xpack][1]
    - `docker-elk/logstash/pipeline/logstash.conf`: configures input, output and filters.
- When data is not appearing, check the logs of the `logstash` container

[1]: https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-xpack.html

### Related docker commands

- Change docker data directory location: https://stackoverflow.com/a/52018760
- Remove persisted data: `docker-compose down -v`
- Troubleshoot container: `docker-compose logs CONTAINER_NAME`
- Display docker usage: `docker system df -v`

## References

- [Logstash](https://www.elastic.co/guide/en/logstash/master/introduction.html)
- [KQL Reference](https://www.elastic.co/guide/en/kibana/master/kuery-query.html)
- [Filter Plugins List](https://www.elastic.co/guide/en/logstash/current/filter-plugins.html)
- [Data types](https://www.elastic.co/guide/en/elasticsearch/reference/current/sql-data-types.html)
- [Query DSL](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl.html)
- [elastic-builder](https://elastic-builder.js.org/docs/)

## Readings

- [ ] Elasticsearch with Kibana maps: https://spinscale.de/posts/2021-09-01-using-german-highway-api-data-with-kibana-maps.html
- [ ] Elasticsearch EQL: https://logz.io/blog/elasticsearch-queries/
- [ ] Elasticsearch API 101: https://logz.io/blog/elasticsearch-api/
- [ ] How does Elasticsearch work: https://www.knowi.com/blog/what-is-elastic-search/
- [x] Current: setup a CSV file with Logstash: https://www.bmc.com/blogs/elasticsearch-load-csv-logstash/
    - How to setup the pipeline config file: https://www.elastic.co/guide/en/logstash/current/docker-config.html
- [x] Fundamentals introduction: https://logz.io/blog/10-elasticsearch-concepts/
- [x] Fundamentals introduction (2): https://medium.com/velotio-perspectives/elasticsearch-101-fundamentals-core-components-a1fdc6090a5e

## Other

- It is important to define the index pattern after some data has been injected
- When there are multiple configuration in logstash pipelines, logstash will merge them
    - To preserve two different flows, need to use tags

## Examples

- CSV: Using a csv file about movies - https://www.kaggle.com/martinmraz07/oscar-movies/version/2
