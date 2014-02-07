# sbt-play-raml

sbt-play-raml is a sbt plugin bringing RAML style route definitions to the playframework.
It replaces the default routes definition syntax used in `conf/routes` with RAML (http://ramel.org).

## Installation

The library is still under heavy development. The library is not yet published (you need to publish it localy). Feel free to try it out or contribute.

**1.** Add the following lines to your sbt plugins file `project/plugins.sbt`:
```
addSbtPlugin("com.scalableminds.raml" %% "sbt-play-raml" % "0.1-SNAPSHOT")
```

**2.** Tell play where to find the compiled routes by adding the following line to your `configuration.conf`:
```
application.router="api.Routes"
````

**3.** Create your RAML routes file in `conf/api.raml` (the file extension MUST be .raml and the file name must match the path of application.router):
```
#%RAML 0.8

title: My first API
version: v1

/:
  get:
    description: |
      controllers.Application.index

/assets:
  /{file}:
    get:
      description: |
        controllers.Assets.at(path="/public", file)
```
