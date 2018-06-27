
# gform

[ ![Download](https://api.bintray.com/packages/hmrc/releases/gform/images/download.svg) ](https://bintray.com/hmrc/releases/gform/_latestVersion)

Gforms is a SaaS for authoring and running form submission journeys.
 
This gform backend persists form templates in a mongo database.  It also persists incomplete form submissions in save4later.
   
When forms are submitted, they the submission data is written into a PDF file and sent to DMS voia GIS using fileupload.
 
 ### Running gform locally with service manager
 
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
        
Upload a whitelisted user:

    curl -X POST http://localhost:9196/gform/white-list/users/insert -H "Content-Type: application/json" -d '"test@test.com"'

Aside from a local service, the backend needs to be accessed through the frontend proxy, to try this locally:

    curl -s http://localhost:9195/submissions/test-only/proxy-to-gform/gform/formtemplates -H "Content-Type: application/json" -H "X-requested-with: foo" -d '@sample-data/template-aaa999.json'
    
(Note that you will need to have configured your local gform-frontend for test only routes and CSRF bypass, as in for example app-config-dev

## Getting started using postman

1. Start the gform and gform-frontend microservices, and any dependencies, as per "Running gform locally with service manager" above.
2. Use postman to post form JSON to the gform microservice.  If using postman, you can import the settings and a sample form definition from [here](https://www.getpostman.com/collections/e77f465bb51501554e15").  Go to the Collections tab on the left and click on "POST a sample gform" under the "gform" folder.
3. Click on the "Body" tab found under the URL box.  This contains the JSON for a working sample form.  Change the _id and name at the top of the sample form to your own unique ID e.g. change test-form-CHANGE-MY-ID to mytest-form-ND.
4. Click the Send button to send your form specification to gforms running in an MDTP environment.  If you get a status 204 back with no response this means the form has been successfully validated and is ready to use.
5. Ensure you are connected to the VPN then access the form in an MDTP environment via the following URL - but note you must update the form ID at the end of this link to the ID that you set in step (3) above.  Don't need to set anything in auth wizard just click Submit:
  https://<MDTP environment host>/auth-login-stub/gg-sign-in?continue=https%3A%2F%2F<MDTP environment host>%2Fsubmissions%2Fnew-form%2Fsample-XXX
6. Once you can see your new form is working you can refer to the specification and re-post updates to your form JSON to tailor your test form to your requirements.  Most types of updates are applied instantly to journeys in-progress but some will require you to re-start the journey.  If you make any mistakes you will get a 400 Bad Request with details of the error in the body.
  You may want to consider using an online JSON editor like https://jsonblob.com/, https://jsoneditoronline.org/ or an editor like https://atom.io/ to simplify authoring your JSON form definitions and copy/paste to postman to apply changes. You can get a feel for the gform template JSON structure by referring to the specification and [current examples](https://github.com/hmrc/gform-templates).


### Using legacy-eeitt-auth?
> If you are working on an EEITT form and using { "authModule": "legacyEEITTAuth" }
> then you will be prompted to login with an enrolled reference number and postcode.
> Pick a test user to use for the correct regime (note 3rd and 4th characters of the ID)
> from the list of test users at https://github.com/hmrc/gform/blob/master/sample-data/EEITTTestUsers.txt.


## APIs

##### POST /formtemplates
Adds a new gform template

##### GET /formtemplates
Gets a list of all gform templates stored in this environment

##### GET /formtemplates/:formTemplateId
Gets the gform template with the given ID (parsed internal format)

##### GET /formtemplates/:formTemplateId/raw
Gets the gform template with the given ID (raw format as originally POSTed)

##### DELETE /formtemplates/:formTemplateId
Deletes the gform template with the given ID


## Internal APIs

> The APIs below are intended for internal use by gforms microservices only and are subject to frequent changes

##### POST /new-form/:formTemplateId/:userId
Create a new form instance for the given form template and user

##### PUT /forms/:formId
Update the given form instance

##### GET /forms/:formId
Get the given form instance

##### GET /forms/:formId/validate-section/:sectionNumber
Get the results of validating the given section of the given form instance

##### DELETE /forms/:formId/deleteFile/:fileId
Delete the uploaded file from the given form instance

##### POST/forms/:formId/submission
Execute submission of the given form instance to the configured destination

##### GET /forms/:formId/submission
Get the submission status of the given form instance


### License

This code is open source software licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html).
  
