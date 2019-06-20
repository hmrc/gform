#!/bin/bash                                                                                                                                                                                
gformsDown.sh
docker run -p 27017:27017 --name mongo -d -v ~/data:/data mongo:3.2.19
sm --start FILE_UPLOAD_FRONTEND -r 1.14.0 --appendArgs '{"FILE_UPLOAD_FRONTEND":["-DProd.mongodb.uri=mongodb://localhost:37017/file-upload-quarantine","-DProd.metrics.enabled=false"]}'
sm --start FILE_UPLOAD -r 1.26.0 --appendArgs '{"FILE_UPLOAD":["-DProd.mongodb.uri=mongodb://localhost:37017/file-upload"]}'
sm --start GFORM_ALL -r
