# Jenkinsfile log

This log is in chronological order from top to bottom

---

Installing latest Jenkins using Docker.

Running commands:

```
docker pull jenkins/jenkins
docker run -p 9000:8080 -p 50000:50000 -v jenkins_home:/var/jenkins_home jenkins/jenkins
```

Reading the document here:

- A: https://jenkins.io/doc/book/pipeline/jenkinsfile/

Before starting, I first go to read:

- B: https://jenkins.io/doc/book/pipeline/getting-started/

Created a new Pipeline in the Jenkins dashboard. Added the "Hello World" declarative pipeline example and run it, checking it works.

Finished reading the article B. Read a few pipelines examples. Tried to build a Pipeline with a plugin (ansiColor) but it wasn't installed, so the build failed. Learned how to install the plugin and then the pipeline worked.

Read article A, which describes in depth the possibilities with declarative and scripted pipelines, with a lot of examples.
