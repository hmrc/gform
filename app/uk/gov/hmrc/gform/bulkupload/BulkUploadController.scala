/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bulkupload

import play.api.mvc.{ Action, ControllerComponents }
import scala.concurrent.Future
import uk.gov.hmrc.gform.controllers.BaseController

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, FormComponentId, FormTemplateId }

class BulkUploadController(bulkUploadService: BulkUploadService[Future], cc: ControllerComponents)(implicit
  ec: ExecutionContext
) extends BaseController(cc) {

  def uploadXlsx(formTemplateId: FormTemplateId, formComponentId: FormComponentId): Action[BulkUploadObjectStoreData] =
    Action.async(parse.json[BulkUploadObjectStoreData]) { implicit request =>
      bulkUploadService.uploadXlsx(request.body).map(_ => NoContent)
    }

  def generateXlsx(formTemplateId: FormTemplateId, formComponentId: FormComponentId): Action[BulkUpload] =
    Action(parse.json[BulkUpload]) { request =>
      Ok(bulkUploadService.generateXlsx(request.body))
        .as("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        .withHeaders(
          CONTENT_DISPOSITION -> s"""attachment; filename="${formTemplateId.value}-${formComponentId.value}.xlsx""""
        )
    }
}
