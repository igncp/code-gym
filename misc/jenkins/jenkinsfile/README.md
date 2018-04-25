# Jenkinsfile

Pipeline is a set of plugins (installed by default in the recommended plugins). Available for v2 and also >v1.6 (but may not work correctly). Created a new Pipeline in the Jenkins dashboard.

The DSL for Scripted Pipeline is a limited version of Groovy. It can be directly in Jenkins or in a Jenkinsfile.

Keywords so far:

- pipeline: At the top of the script
- agent: In this case is `any`
- stages: First level within `pipeline`
- stage('Stage 1'): Inside `stages`
- steps: Inside a stage
- echo 'Hello world!' : Inside a step

There are two types of pipelines: **Scripted** and **Declarative**

There is built-in documentation at: JENKINS_URL/pipeline-syntax/

You can use the `sh` keyword to run commands, e.g. `sh "mkdir foo"`

You can define variables with the keyword `def`

Example of how to get the commit in a variable:

```
shortCommit = sh(returnStdout: true, script: "git log -n 1 --pretty=format:'%h'").trim()
```

## References

- https://jenkins.io/doc/pipeline/steps/
- https://jenkins.io/doc/book/pipeline/syntax/
