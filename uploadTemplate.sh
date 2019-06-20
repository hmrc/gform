#!/bin/sh

curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@'$1
