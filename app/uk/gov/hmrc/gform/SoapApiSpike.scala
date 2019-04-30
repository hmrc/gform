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

package uk.gov.hmrc.gform

object SoapApiSpike extends App {

  private val host = "https://testinfogateway.ofsted.gov.uk/OnlineOfsted/GatewayOOServices.svc"
  private val req = <IsAlive xmlns="https://api.authorize.net/soap/v1/"/>

  new SoapClient().sendMessage(host, req)
}

import java.time.LocalDateTime

import scala.xml.{ Elem, XML }

class SoapClient {
  private def error(msg: String) =
    println("SoapClient error: " + msg)

  def sendMessage(host: String, req: Elem): Option[Elem] = {
    val url = new java.net.URL(host)
    val outs: Array[Byte] = request.getBytes
    val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
    try {
      conn.setRequestMethod("POST")
      conn.setDoOutput(true)
      conn.setRequestProperty("Content-Length", outs.length.toString)
      conn.setRequestProperty("Content-Type", "text/xml")
      conn.getOutputStream.write(outs)
      conn.getOutputStream.close
      Some(XML.load(conn.getInputStream))
    } catch {
      case e: Exception =>
        error("post: " + e)
        error("post:" + scala.io.Source.fromInputStream(conn.getErrorStream).mkString)
        None
    }
  }

  private val request =
    s"""<soap-env:Envelope xmlns:soap-env="http://www.w3.org/2003/05/soap-envelope">
       |   <soap-env:Header xmlns:wsa="http://www.w3.org/2005/08/addressing">
       |      <wsa:Action>http://tempuri.org/IGatewayOOServices/GetData</wsa:Action>
       |      <wsa:MessageID>urn:uuid:a4541cac-b85e-42a8-9332-47232e8c8243</wsa:MessageID>
       |      <wsa:To xmlns:ns2="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" ns2:Id="id-e243a685-5cb3-4c5c-b3b8-8114a62f479c">https://testinfogateway.ofsted.gov.uk/OnlineOfsted/GatewayOOServices.svc</wsa:To>
       |      <wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">
       |         <ns0:Timestamp xmlns:ns0="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" ns0:Id="id-5ade777c-33dd-43a5-8290-07282034813b">
       |            <ns0:Created>${LocalDateTime.now}</ns0:Created>
       |            <ns0:Expires>${LocalDateTime.now.plusMinutes(5)}</ns0:Expires>
       |         </ns0:Timestamp>
       |         <wsse:BinarySecurityToken xmlns:ns1="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd" EncodingType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-soap-message-security-1.0#Base64Binary" ValueType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-x509-token-profile-1.0#X509v3" ns1:Id="id-d1805dbc-c8d1-42d7-9921-479f4c3f8a9b">
       |		 MIIG4zCCBcu... (Base64 encoded public key representation)
       |		 </wsse:BinarySecurityToken>
       |         <Signature xmlns="http://www.w3.org/2000/09/xmldsig#">
       |            <SignedInfo>
       |               <CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#" />
       |               <SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />
       |               <Reference URI="#id-e243a685-5cb3-4c5c-b3b8-8114a62f479c">
       |                  <Transforms>
       |                     <Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#" />
       |                  </Transforms>
       |                  <DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />
       |                  <DigestValue>h0If3gzp4Ah/dqfS8dXD5BHXbOU=</DigestValue>
       |               </Reference>
       |               <Reference URI="#id-5ade777c-33dd-43a5-8290-07282034813b">
       |                  <Transforms>
       |                     <Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#" />
       |                  </Transforms>
       |                  <DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />
       |                  <DigestValue>B1i1oT5nyjfRO0Lc02ZzgEryMqU=</DigestValue>
       |               </Reference>
       |            </SignedInfo>
       |            <SignatureValue>SIGNATURE-VALUE</SignatureValue>
       |            <KeyInfo>
       |               <wsse:SecurityTokenReference>
       |                  <wsse:Reference ValueType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-x509-token-profile-1.0#X509v3" URI="#id-d1805dbc-c8d1-42d7-9921-479f4c3f8a9b" />
       |               </wsse:SecurityTokenReference>
       |            </KeyInfo>
       |         </Signature>
       |         <wsse:UsernameToken>
       |            <wsse:Username>USERNAME-HERE</wsse:Username>
       |            <wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText">tdLK!PEV8}Gb5CM</wsse:Password>
       |         </wsse:UsernameToken>
       |      </wsse:Security>
       |   </soap-env:Header>
       |   <soap-env:Body>
       |      <ns0:GetData xmlns:ns0="http://tempuri.org/">
       |         <ns0:Service>GetNewURN</ns0:Service>
       |         <ns0:InputParameters>&lt;Parameters&gt;&lt;IDs&gt;&lt;ID&gt;EY&lt;/ID&gt;&lt;/IDs&gt;&lt;/Parameters&gt;</ns0:InputParameters>
       |      </ns0:GetData>
       |   </soap-env:Body>
       |</soap-env:Envelope>""".stripMargin
}
