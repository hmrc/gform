
# gform

[![Build Status](https://travis-ci.org/hmrc/gform.svg?branch=master)](https://travis-ci.org/hmrc/gform) [ ![Download](https://api.bintray.com/packages/hmrc/releases/gform/images/download.svg) ](https://bintray.com/hmrc/releases/gform/_latestVersion)

### Uploading sample data

To upload the sample schema to a local gform service:

    curl http://localhost:9196/gform/schemas -H "Content-Type: application/json" -d '@sample-data/schema.json'

Similarly, to upload a form template:

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template.json'

View data with, for example:

    curl http://localhost:9196/gform/schemas

    curl http://localhost:9196/gform/formtemplates

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
    