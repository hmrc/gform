
# gform

[ ![Download](https://api.bintray.com/packages/hmrc/releases/gform/images/download.svg) ](https://bintray.com/hmrc/releases/gform/_latestVersion)

### Uploading sample data

Upload a form template:

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template-aaa999.json'

Upload eeitt test data: 
    
    curl --data-binary '@sample-data/EEITTTestUsers.txt' http://localhost:9191/eeitt/etmp-data/live/business-users
        
Aside from a local service, the backend needs to be accessed through the frontend proxy, to try this locally:

    curl http://localhost:9195/summary/test-only/proxy-to-gform/gform/schemas -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/schema.json'

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
  
