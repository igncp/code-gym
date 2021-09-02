Some notes and scripts to get more experience with the ELK stack

- Setup:
    - https://github.com/deviantony/docker-elk

- Kibana: http://localhost:5601/
- Logstash TCP: http://localhost:5000/
- Logstash Monitoring API: http://localhost:9600/
- Elasticsearch API: http://localhost:9200/

## Tutorials

- Current: setup a CSV file with Logstash: https://www.bmc.com/blogs/elasticsearch-load-csv-logstash/
    - How to setup the pipeline config file: https://www.elastic.co/guide/en/logstash/current/docker-config.html

## Notes

- Default user: `elastic` / `changeme`
- For injecting data: `sudo pacman -S gnu-netcat` (installs `nc`)
    - You can also import files manually
- Convert string to number in Logstash: https://discuss.elastic.co/t/logstash-convert-string-to-integer/96325
- Pipelines Management GUI: `Kibana > Management > Ingest Node Pipelines`

### Related docker commands

- Change docker data directory location: https://stackoverflow.com/a/52018760
- Remove persisted data: `docker-compose down -v`
- Display docker usage: `docker system df -v`

## References

- [Logstash](https://www.elastic.co/guide/en/logstash/master/introduction.html)
- [KQL Reference](https://www.elastic.co/guide/en/kibana/master/kuery-query.html)

## Other

- It is important to define the index pattern after some data has been injected
