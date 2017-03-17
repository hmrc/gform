
# bforms

[![Build Status](https://travis-ci.org/hmrc/bforms.svg?branch=master)](https://travis-ci.org/hmrc/bforms) [ ![Download](https://api.bintray.com/packages/hmrc/releases/bforms/images/download.svg) ](https://bintray.com/hmrc/releases/bforms/_latestVersion)

### Uploading sample data

To upload the sample schema to a local bforms service:

    curl http://localhost:9196/bforms/schemas -H "Content-Type: application/json" -d '@sample-data/schema.json' 

Similarly, to upload a form template:

    curl http://localhost:9196/bforms/formtemplates -H "Content-Type: application/json" -d '@sample-data/template.json' 

View data with, for example:

    curl http://localhost:9196/bforms/schemas

    curl http://localhost:9196/bforms/formtemplates

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
    