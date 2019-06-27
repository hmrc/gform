/*
 * Copyright 2019 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, SingleQuoteReplacementLexer, TemplateType }

class HandlebarsTemplateProcessorSpec extends Spec {
  "apply" must "work with JSON" in {
    RealHandlebarsTemplateProcessor(
      "I am a {{name}} template",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.JSON) shouldBe "I am a handlebars template"
  }

  it must "work with XML" in {
    RealHandlebarsTemplateProcessor(
      "<foo>I am a {{name}} template</foo>",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.XML) shouldBe "<foo>I am a handlebars template</foo>"
  }

  it must "work with plain text" in {
    RealHandlebarsTemplateProcessor(
      "I am a {{name}} template",
      HandlebarsTemplateProcessorModel("""{"name" : "handlebars"}"""),
      TemplateType.XML) shouldBe "I am a handlebars template"
  }

  "JsonEscapingStrategy un-escaping" must "work with apostrophes" in {
    RealHandlebarsTemplateProcessor(
      "string with an {{apostrophe}}",
      HandlebarsTemplateProcessorModel("""{"apostrophe" : "'"}"""),
      TemplateType.JSON) shouldBe "string with an '"
  }

  it must "work with a backslash" in {
    RealHandlebarsTemplateProcessor("""string with a \""", HandlebarsTemplateProcessorModel.empty, TemplateType.JSON) shouldBe """string with a \"""
  }

  it must "work with two backslashes" in {
    RealHandlebarsTemplateProcessor("""string with two \\""", HandlebarsTemplateProcessorModel.empty, TemplateType.JSON) shouldBe """string with two \\"""
  }
}

object ProcessStructuredData extends App {
  import cats.syntax.either._
  import play.api.libs.json.Json
  import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
  import scala.xml.XML

  val template = SingleQuoteReplacementLexer(
    "<ApplicationForms> <ApplicationForm> <FormType> <EY2> {{#if registeredPersonReferenceCNP}} <Provider> <RegistrationId>{{registeredPersonReferenceCNP_regid}}</RegistrationId> <ProvisionTypeId>{{match ^('0') => '33'; ('1') => '32'; (*) => '99'^ (stripCommas serviceType)}}</ProvisionTypeId> <SettingName>{{registeredPersonNameCNP2}}</SettingName> <URNReferenceNumber>{{registeredPersonReferenceCNP}}</URNReferenceNumber> <SettingAddress> <Address1>{{registeredPersonAddressCNP2.street1}}</Address1> <Address2>{{registeredPersonAddressCNP2.street2}}</Address2> <Town>{{registeredPersonAddressCNP2.street3}}</Town> <County>{{registeredPersonAddressCNP2.street4}}</County> <PostCode>{{registeredPersonAddressCNP2.postcode}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> </SettingAddress> </Provider> {{/if}} {{#if settingNameCNPAlreadyRegSRN}} <Provider> <RegistrationId>{{settingURNAlreadyRegSRN_regid}}</RegistrationId> <ProvisionTypeId>{{match ^('0') => '33'; ('1') => '32'; (*) => '99'^ (stripCommas serviceType)}}</ProvisionTypeId> <SettingName>{{settingURNAlreadyRegSRN}}</SettingName> <URNReferenceNumber>{{settingNameCNPAlreadyRegSRN}}</URNReferenceNumber> <SettingAddress> <Address1>{{settingAddressCNPAlreadyRegSRN.street1}}</Address1> <Address2>{{settingAddressCNPAlreadyRegSRN.street2}}</Address2> <Town>{{settingAddressCNPAlreadyRegSRN.street3}}</Town> <County>{{settingAddressCNPAlreadyRegSRN.street4}}</County> <PostCode>{{settingAddressCNPAlreadyRegSRN.postcode}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> </SettingAddress> </Provider> {{#each settingURNAlreadyRegSRN2}} <Provider> <RegistrationId>{{settingURNAlreadyRegSRN2_regid}}</RegistrationId> <ProvisionTypeId>{{match ^('0') => '33'; ('1') => '32'; (*) => '99'^ (stripCommas serviceType)}}</ProvisionTypeId> <SettingName>{{settingURNAlreadyRegSRN2}}</SettingName> <URNReferenceNumber>{{lookup @root.settingNameSRNAdditional @index}}</URNReferenceNumber> <SettingAddress> <Address1>{{lookup @root.settingAddressSRNAdditional.street1 @index}}</Address1> <Address2>{{lookup @root.settingAddressSRNAdditional.street2 @index}}</Address2> <Town>{{lookup @root.settingAddressSRNAdditional.street3 @index}}</Town> <County>{{lookup @root.settingAddressSRNAdditional.street4 @index}}</County> <PostCode>{{lookup @root.settingAddressSRNAdditional.postcode @index}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> </SettingAddress> </Provider> {{/each}} {{/if}} <ConnectionWithRegistration> {{#if soleTrader}} <SoleOwnerCC>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas soleTrader)}}</SoleOwnerCC> {{/if}} {{#if provideChildcareMainPurpose}} <MakingUpOrgCC>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas provideChildcareMainPurpose)}}</MakingUpOrgCC> {{/if}} {{#if roleInOrganisationNonDomestic}} <RoleIsDirector>{{match ^('0') => 'Yes'; (*) => 'Null'^ (stripCommas roleInOrganisationNonDomestic.choice)}}</RoleIsDirector> {{/if}} {{#if roleInOrganisationNonDomestic}} <RoleIsPartner>{{match ^('1') => 'Yes'; (*) => 'Null'^ (stripCommas roleInOrganisationNonDomestic.choice)}}</RoleIsPartner> {{/if}} {{#if roleInOrganisationNonDomestic}} <RoleIsCommitteeMember>{{match ^('2') => 'Yes'; (*) => 'Null'^ (stripCommas roleInOrganisationNonDomestic.choice)}}</RoleIsCommitteeMember> {{/if}} {{#if roleInOrganisationNonDomestic}} <RoleIsOther>{{match ^('3') => 'Yes'; (*) => 'Null'^ (stripCommas roleInOrganisationNonDomestic.choice)}}</RoleIsOther> {{/if}} <RoleOther>{{roleInOrganisationNonDomestic.revealed.specifyOther}}</RoleOther> {{#if nominatedIndividual}} <NominatedPersonForOrg>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas nominatedIndividual)}}</NominatedPersonForOrg> {{/if}} {{#if manageDayToDay}} <DirectlyManage>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas manageDayToDay)}}</DirectlyManage> {{/if}} {{#if adultChildRatio}} <MemberOfStaff>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas adultChildRatio)}}</MemberOfStaff> {{/if}} {{#if lookingAfter}} <LookingAfterAsCC>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas lookingAfter)}}</LookingAfterAsCC> {{/if}} {{#if notLookingAfter}} <AtCCPremises>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas notLookingAfter)}}</AtCCPremises> {{/if}} </ConnectionWithRegistration> <Personal> <IndividualId>{{either indid 0}}</IndividualId> <Title>{{match ^('0') => 'Mr'; ('1') => 'Mrs'; ('2') => 'Miss'; ('3') => 'Ms'; ('4') => 'Other';^ (stripCommas @root.title.choice)}}</Title> <OtherTitle>{{@root.title.revealed.personOtherTitle}}</OtherTitle> <FirstName>{{firstName}}</FirstName> <MiddleNames></MiddleNames> <Surname>{{surname}}</Surname> <SurnameAtBirth>{{surnameAtBirth}}</SurnameAtBirth> <AnyOtherNames>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas anyOtherNames)}}</AnyOtherNames> <OtherNames> {{#each prevSurname}} <OtherName> <FirstName>{{lookup @root.prevFirstName @index}}</FirstName> <Surname>{{.}}</Surname> <DateFrom>{{lookup @root.dateKnownFrom.year @index}}-{{lookup @root.dateKnownFrom.month @index}}-{{lookup @root.dateKnownFrom.day @index}}T00:00:00</DateFrom> <DateTo>{{lookup @root.dateKnownUntil.year @index}}-{{lookup @root.dateKnownUntil.month @index}}-{{lookup @root.dateKnownUntil.day @index}}T00:00:00</DateTo> <ID>{{@index}}</ID> </OtherName> {{/each}} </OtherNames> <DateOfBirth>{{DoB.year}}-{{DoB.month}}-{{DoB.day}}T00:00:00</DateOfBirth> <PlaceOfBirthTown></PlaceOfBirthTown> <PlaceOfBirthCounty></PlaceOfBirthCounty> <BornInUK>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas bornUkNo.choice)}}</BornInUK> <OtherCountry>{{bornUkNo.revealed.bornUkSpecify}}</OtherCountry> <Nationality></Nationality> <Sex>U</Sex> <LivedOutsideUK>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas livedOutsideUK)}}</LivedOutsideUK> <CurrentAddressDetails> <Address1>{{currentAddress.street1}}</Address1> <Address2>{{currentAddress.street2}}</Address2> <Town>{{currentAddress.street3}}</Town> <County>{{currentAddress.street4}}</County> <PostCode>{{currentAddress.postcode}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> {{#if sinceWhenCurrentAddress}} <FromDate>{{sinceWhenCurrentAddress.year}}-{{sinceWhenCurrentAddress.month}}-{{sinceWhenCurrentAddress.day}}T00:00:00</FromDate> {{/if}} </CurrentAddressDetails> <AnyOtherAddresses>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas otherAddresses)}}</AnyOtherAddresses> <OtherAddresses> {{#each uKAddresses.street1}} <Address> <ID>{{@index}}</ID> <Address1>{{.}}</Address1> <Address2>{{lookup @root.uKAddresses.street2 @index}}</Address2> <Town>{{lookup @root.uKAddresses.street3 @index}}</Town> <County>{{lookup @root.uKAddresses.street4 @index}}</County> <PostCode>{{lookup @root.uKAddresses.postcode @index}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> {{#with (lookup @root.dateMovedInUK @index) as |date|}} {{#if date}} <FromDate>{{lookup @root.date.year @index}}-{{lookup @root.date.month @index}}-{{lookup @root.date.day @index}}T00:00:00</FromDate> {{/if}} {{/with}} {{#with (lookup @root.dateMovedOutUK @index) as |date|}} {{#if date}} <ToDate>{{lookup @root.date.year @index}}-{{lookup @root.date.month @index}}-{{lookup @root.date.day @index}}T00:00:00</ToDate> {{/if}} {{/with}} </Address> {{/each}} </OtherAddresses> <DisclosureNumber>{{numberDBSNew}}</DisclosureNumber> </Personal> <Contact> <MainContactNumber>Unknown</MainContactNumber> <EmailAddress>{{email}}</EmailAddress> <DaytimeContactNumber>{{phoneNumber}}</DaytimeContactNumber> <MobileContactNumber></MobileContactNumber> </Contact> <PastRegistration> <OfstedReg>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas registeredToProvideChildcare)}}</OfstedReg> <OtherRegBodyUK>Null</OtherRegBodyUK> <OtherRegBodyEU>Null</OtherRegBodyEU> <ResidentEnglandFiveYears>Null</ResidentEnglandFiveYears> {{#if livedOrWorkedMilitaryBaseOutsideEngland}} <ResidentUkOverseasFiveYears>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas livedOrWorkedMilitaryBaseOutsideEngland)}}</ResidentUkOverseasFiveYears> {{/if}} </PastRegistration> <PreviousExperience> <RelaventTrainQual>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas childcareTraining)}}</RelaventTrainQual> <TrainQualDetails> {{#each titleOfCourse}} <TrainQual> <CourseTitle>{{.}}</CourseTitle> {{#with (lookup @root.relevantFromDate @index) as |dateFrom|}} {{#if dateFrom}} <DateFrom>{{lookup @root.dateFrom.year @index}}-{{lookup @root.dateFrom.month @index}}-{{lookup @root.dateFrom.day @index}}T00:00:00</DateFrom> {{/if}} {{/with}} {{#with (lookup @root.relevantToDate @index) as |dateTo|}} {{#if dateTo}} <DateTo>{{lookup @root.dateTo.year @index}}-{{lookup @root.dateTo.month @index}}-{{lookup @root.dateTo.day @index}}T00:00:00</DateTo> {{/if}} {{/with}} {{#with (lookup @root.qualification @index) as |qual|}} {{#if qual}} <Qualification>{{lookup @root.qualification @index}}</Qualification> {{/if}} {{/with}} {{#with (lookup @root.dateGained @index) as |dateGain|}} {{#if dateGain}} <DateGained>{{lookup @root.dateGain.year @index}}-{{lookup @root.dateGain.month @index}}-{{lookup @root.dateGain.day @index}}T00:00:00</DateGained> {{/if}} {{/with}} {{#with (lookup @root.awardingBody @index) as |awardBod|}} {{#if awardBod}} <AwardingBody>{{lookup @root.awardingBody @index}}</AwardingBody> {{/if}} {{/with}} <ID>{{@index}}</ID> </TrainQual> {{/each}} </TrainQualDetails> <AnyFirstAidCert>Null</AnyFirstAidCert> <LocalAuthorityTrainingOrgName></LocalAuthorityTrainingOrgName> </PreviousExperience> <Health> <AnyIllnesses>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas seriousIllness)}}</AnyIllnesses> <AnyAdmissions>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas hospitalAdmission)}}</AnyAdmissions> <Treatment>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas currentlyBeingTreated)}}</Treatment> </Health> <Referees> <FirstReferenceAddress> <Title>{{match ^('0') => 'Mr'; ('1') => 'Mrs'; ('2') => 'Miss'; ('3') => 'Ms'; (*) => 'Other'; ^ (stripCommas @root.referee2Title.choice)}}</Title> <OtherTitle>{{@root.referee2Title.revealed.referee2TitleOther}}</OtherTitle> <FirstName>{{referee1FirstName}}</FirstName> <Surname>{{referee1Surname}}</Surname> <ReferenceAddress> <Address1>{{referee1Address.street1}}</Address1> <Address2>{{referee1Address.street2}}</Address2> <Town>{{referee1Address.street3}}</Town> <County>{{referee1Address.street4}}</County> <PostCode>{{referee1Address.postcode}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> </ReferenceAddress> <TelephoneNumber>{{referee1Phone}}</TelephoneNumber> <Email>{{referee1Email}}</Email> <Relationship>{{referee1HowKnown}}</Relationship> <ReferenceEnclosed>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas referee1ReferenceEnclosed)}}</ReferenceEnclosed> <TimeKnownMonths></TimeKnownMonths> <TimeKnownYears>{{referee2SinceWhenKnownYears_copy_1}}</TimeKnownYears> </FirstReferenceAddress> <SecondReferenceAddress> <Title>{{match ^('0') => 'Mr'; ('1') => 'Mrs'; ('2') => 'Miss'; ('3') => 'Ms'; (*) => 'Other'; ^ (stripCommas referee2Title_copy_1)}}</Title> <OtherTitle>{{referee2Title_copy_1.revealed.referee2TitleOther_copy_1}}</OtherTitle> <FirstName>{{referee2FirstName}}</FirstName> <Surname>{{referee2Surname}}</Surname> <ReferenceAddress> <Address1>{{referee2Address.street1}}</Address1> <Address2>{{referee2Address.street2}}</Address2> <Town>{{referee2Address.street3}}</Town> <County>{{referee2Address.street4}}</County> <PostCode>{{referee2Address.postcode}}</PostCode> <LocalAuthorityDescription></LocalAuthorityDescription> <LocalAuthorityCode></LocalAuthorityCode> </ReferenceAddress> <TelephoneNumber>{{referee2Phone}}</TelephoneNumber> <Email>{{referee2Email}}</Email> <Relationship>{{referee2HowKnown}}</Relationship> <ReferenceEnclosed>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas referee2ReferenceEnclosed)}}</ReferenceEnclosed> <TimeKnownMonths></TimeKnownMonths> <TimeKnownYears>{{referee2SinceWhenKnownYears}}</TimeKnownYears> </SecondReferenceAddress> </Referees> <SuitabilityAndDisqualification> {{#if circumstancesSuitabilityDisqualification}} <CircumApply>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas circumstancesSuitabilityDisqualification)}}</CircumApply> {{/if}} <CircumApplyDetails></CircumApplyDetails> {{#if involvedSocialServicesOwnChildren}} <SocialService>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas involvedSocialServicesOwnChildren)}}</SocialService> {{/if}} <SocialServiceDetails></SocialServiceDetails> {{#if criminalOffences}} <CriminalConviction>{{match ^('0') => 'Yes'; ('1') => 'No'; (*) => 'Null'^ (stripCommas criminalOffences)}}</CriminalConviction> {{/if}} </SuitabilityAndDisqualification> <ConsentAndDeclaration> <AgreeConsent>1</AgreeConsent> <DateOfBirth>{{DoB.year}}-{{DoB.month}}-{{DoB.day}}T00:00:00</DateOfBirth> </ConsentAndDeclaration> </EY2> </FormType> <FormID>1</FormID> <CreatedBy>hvanker</CreatedBy> <CreatedDate>{{currentTimestamp}}</CreatedDate> <Source>Online</Source> <ParentID>0</ParentID> <URN>{{either registeredPersonReferenceCNP settingNameCNPAlreadyRegSRN}}</URN> <FormMetaData> </FormMetaData> </ApplicationForm> </ApplicationForms>"
  ).toOption.get
  println(template)

  val structuredData = Json.parse("""|{
                                     |  "fields": [
                                     |    {
                                     |      "name": "authYesNo",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "serviceType",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "businessRegisteredCNPAndCDP",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "referenceNumberCNPAlreadyReg",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "connectionToBusinessCNPAlreadyReg",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "provideChildcareMainPurpose",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "roleInOrganisationNonDomestic",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "0"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "nominatedIndividual",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "registeredPersonReferenceCNP",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "RP511244"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "registeredPersonReferenceCNP_regid",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "2172423"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "registeredPersonCNPInfo_copy_3",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "registeredPersonNameCNP2",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "Apple Daycare Ltd"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "registeredPersonAddressCNP2",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "street1",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "DHOON"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line1"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street2",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Epping Green"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line2"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street3",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Hertford"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line3"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street4",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Hertfordshire"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line4"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "uk",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "true"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "uk"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "postcode",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "SG13 8ND"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "postcode"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "country",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": ""
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "country"
                                     |              }
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "adultChildRatio",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "generatedId_4526425c_df63_4b62_978b_3dbc5a3a925c",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "generatedId_baa9b24f_e224_406b_acfa_8a0b43c8dd44",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "title",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "0"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "firstName",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "Lance"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "surname",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "Walton"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "anyOtherNames",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "DoB",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "day",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "25"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "month",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "04"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "year",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "1969"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "bornUkNo",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "0"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "generatedId_10975cf0_c0b7_4ba1_ad50_269bb9308986",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "email",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "lancewalton@mac.com"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "phoneNumber",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "07779 026533"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "addressInfo",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "currentAddress",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "street1",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Stonehills"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line1"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street2",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Tilford Road"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line2"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street3",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Tilford"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line3"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "street4",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "Surrey"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "line4"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "uk",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "true"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "uk"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "postcode",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "GU10 2DE"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "postcode"
                                     |              }
                                     |            },
                                     |            {
                                     |              "name": "country",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": ""
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {
                                     |                "RoboticsXml": "country"
                                     |              }
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "sinceWhenCurrentAddress",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "day",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "01"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "month",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "01"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "year",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "2010"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "otherAddresses",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "livedOutsideUK",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "timeUnaccountedFor",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "prevRegOfsted",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "1"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "prevRegOtherBodyUk",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "1"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "prevRegOtherBodyEU",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "1"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "prevRegMOD",
                                     |      "value": {
                                     |        "ObjectStructure": {
                                     |          "fields": [
                                     |            {
                                     |              "name": "choice",
                                     |              "value": {
                                     |                "TextNode": {
                                     |                  "value": "1"
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            },
                                     |            {
                                     |              "name": "revealed",
                                     |              "value": {
                                     |                "ObjectStructure": {
                                     |                  "fields": []
                                     |                }
                                     |              },
                                     |              "alternativeFieldNames": {}
                                     |            }
                                     |          ]
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "livedOrWorkedMilitaryBaseEngland",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "livedOrWorkedMilitaryBaseOutsideEngland",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "circumstancesSuitabilityDisqualification",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "suitabilityInfo",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "generatedId_135e8ba3_a042_48f8_baf5_ea3c412fae18",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "suitabilityToWorkWithChildren",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "involvedSocialServicesOwnChildren",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "criminalOffences",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "criminalOffenceInfo",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": ""
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "dbsCheck",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "ofstedDBSNew",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "dateDbsCertThreeMonth",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "numberDBSNew",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1234567890123"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "updateServiceNew",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "0,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    },
                                     |    {
                                     |      "name": "isaRegistration",
                                     |      "value": {
                                     |        "TextNode": {
                                     |          "value": "1,"
                                     |        }
                                     |      },
                                     |      "alternativeFieldNames": {}
                                     |    }
                                     |  ]
                                     |}""".stripMargin).as[StructuredFormValue.ObjectStructure]
  val model = HandlebarsTemplateProcessorModel(structuredData)
  println(model.model)

  val payload = RealHandlebarsTemplateProcessor(template, model, TemplateType.XML)
  println(payload)

  println(XML.loadString(payload))
}
