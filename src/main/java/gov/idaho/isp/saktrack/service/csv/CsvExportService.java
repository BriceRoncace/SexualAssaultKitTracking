/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.service.csv;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.UserLogin;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.report.RequestingAgencyReport;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReport;
import java.util.List;

public interface CsvExportService {
  CsvDownloadable exportLifeCycleReport(RequestingAgencyReport report);
  CsvDownloadable exportUnusedKitsReport(CurrentAssignmentReport report);
  CsvDownloadable exportUnreceivedCollectedKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportReceivedCollectedKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportExceedingStatutoryRequirementsReport(StatutoryRequirementReport report);
  CsvDownloadable exportUnsubmittableKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportSubmittedKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportDatabaseKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportDatabaseHitKitsReport(RequestingAgencyReport report);
  CsvDownloadable exportKitsWithQuestionableEventsReport(CurrentAssignmentReport report);

  CsvDownloadable exportKitSearchResults(List<SexualAssaultKit> kits);
  CsvDownloadable exportKitSearchResults(List<SexualAssaultKit> kits, OrganizationType orgType);

  CsvDownloadable exportUserList(List<AbstractUser> users);
  CsvDownloadable exportUserLoginList(List<UserLogin> userLogins);
}
