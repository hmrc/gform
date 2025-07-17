
# gform

[ ![Download](https://api.bintray.com/packages/hmrc/releases/gform/images/download.svg) ](https://bintray.com/hmrc/releases/gform/_latestVersion)

Gform is a SaaS for authoring and running form submission journeys. The components and patterns used to author the journeys are compliant with the GOV.UK Design System.
 
This gform backend persists form templates in a mongo database.  It also persists incomplete form submissions in mongo forms collection.
   
When forms are submitted the submission data can be sent to any number of configured destinations. Additional destinations can be added as required and made available to all future forms.

The destinations that are currently supported are as follows.
- DMS queue as a PDF file
- DMS queue as an xml file for robotics to process
- API within HMRC core systems. Calls can be made via EIS - DES, HIP etc.
- Date Store as a json file containing all of the data in the submission (this can be configured with a json schema to define the contract between Gform and the conduming system)
- Info Archive as a PDF file
 
## License

This code is open source software licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html).
  
