package gov.idaho.isp.saktrack.service.csv;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.report.RequestingAgencyReport;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReport;
import gov.idaho.isp.saktrack.user.AbstractUser;
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
}
