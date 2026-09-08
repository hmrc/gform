#!/usr/bin/env bash
# DMS via object-store: file-upload (8898) is decommissioned and not in the sm2 catalogue.
sbt "run -Dplay.http.router=testOnlyDoNotUseInAppConf.Routes -Dobject-store.enable-dms-services=true"