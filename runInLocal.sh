#!/usr/bin/env bash
sbt ~run -Dplay.http.router=testOnlyDoNotUseInAppConf.Routes --jvm-debug 5006