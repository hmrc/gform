
# gform

[ ![Download](https://api.bintray.com/packages/hmrc/releases/gform/images/download.svg) ](https://bintray.com/hmrc/releases/gform/_latestVersion)

Gform is a system for offering more straightforward tax forms online using based on a template of the form in json.
 
This gform backend persists form templates in a mongo database.  It also persists incomplete form submissions in save4later.
   
When forms are submitted, they the submissio data is written into a PDF file and sent to DMS voia GIS using fileupload.
 
 ### Running Gform Locally with Service Manager
 
 When running locally: 
  
     sm --start GFORM_DEP -f
     
 runs all dependent services of gform. 
 
 when using gform: 
     
     sm --start GFORM_ALL -f
     
 run dependencies and gform.
 
## Uploading sample data

Upload a form template:

    curl http://localhost:9196/gform/formtemplates -H "Content-Type: application/json" -d '@sample-data/template-aaa999.json'

Upload eeitt test data: 
    
    curl --data-binary '@sample-data/EEITTTestUsers.txt' http://localhost:9191/eeitt/etmp-data/live/business-users
        
Aside from a local service, the backend needs to be accessed through the frontend proxy, to try this locally:

    curl -s http://localhost:9195/submissions/test-only/proxy-to-gform/gform/formtemplates -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/template-aaa999.json'
    
(Note that you will need to have configured your local gform-frontend for test only routes and CSRF bypass, as in for example app-config-dev

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
  
