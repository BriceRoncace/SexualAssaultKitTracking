package gov.idaho.isp.saktrack.service.csv;

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReport;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReportGroup;
import gov.idaho.isp.saktrack.report.CurrentAssignmentReportRow;
import gov.idaho.isp.saktrack.report.RequestingAgencyReport;
import gov.idaho.isp.saktrack.report.RequestingAgencyReportGroup;
import gov.idaho.isp.saktrack.report.RequestingAgencyReportRow;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReport;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReportGroup;
import gov.idaho.isp.saktrack.report.StatutoryRequirementReportRow;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.Arrays;
import org.junit.Assert;
import org.junit.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class CsvExportServiceTest {
  private final String NEWLINE = "\n";
  private final CsvExportService exporter = new CsvExportServiceImpl();

  private final RequestingAgencyReport mockRequestingAgencyReport = mock(RequestingAgencyReport.class);
  private final RequestingAgencyReportGroup mockRequestingAgencyReportGroup = mock(RequestingAgencyReportGroup.class);
  private final RequestingAgencyReportRow mockRequestingAgencyReportRow = mock(RequestingAgencyReportRow.class);

  private final CurrentAssignmentReport mockCurrentAssignmentReport = mock(CurrentAssignmentReport.class);
  private final CurrentAssignmentReportGroup mockCurrentAssignmentReportGroup = mock(CurrentAssignmentReportGroup.class);
  private final CurrentAssignmentReportRow mockCurrentAssignmentReportRow = mock(CurrentAssignmentReportRow.class);

  private final StatutoryRequirementReport mockStatutoryRequirementReport = mock(StatutoryRequirementReport.class);
  private final StatutoryRequirementReportGroup mockStatutoryRequirementReportGroup = mock(StatutoryRequirementReportGroup.class);
  private final StatutoryRequirementReportRow mockStatutoryRequirementReportRow = mock(StatutoryRequirementReportRow.class);

  public CsvExportServiceTest() {
    when(mockRequestingAgencyReport.getGroups()).thenReturn(Arrays.asList(mockRequestingAgencyReportGroup));
    when(mockRequestingAgencyReportGroup.getRows()).thenReturn(Arrays.asList(mockRequestingAgencyReportRow));
    when(mockRequestingAgencyReportRow.getName()).thenReturn("Name");
    when(mockRequestingAgencyReportRow.getSerialNumber()).thenReturn("012345");
    when(mockRequestingAgencyReportRow.getLastModifiedBy()).thenReturn("Modifier");
    when(mockRequestingAgencyReportRow.getLastModified()).thenReturn(LocalDateTime.of(2017, Month.FEBRUARY, 16, 10, 22));
    when(mockRequestingAgencyReportRow.getCollectionDate()).thenReturn(LocalDate.of(2017, Month.JANUARY, 1));
    when(mockRequestingAgencyReportRow.getTimeAtMedical()).thenReturn(Long.valueOf(1));
    when(mockRequestingAgencyReportRow.isAtMedical()).thenReturn(false);
    when(mockRequestingAgencyReportRow.getTimeInTransit()).thenReturn(Long.valueOf(2));
    when(mockRequestingAgencyReportRow.isInTransit()).thenReturn(false);
    when(mockRequestingAgencyReportRow.getTimeAtLawEnforcement()).thenReturn(Long.valueOf(3));
    when(mockRequestingAgencyReportRow.isAtLawEnforcement()).thenReturn(false);
    when(mockRequestingAgencyReportRow.getTimeAtLab()).thenReturn(Long.valueOf(4));
    when(mockRequestingAgencyReportRow.isAtLab()).thenReturn(true);
    when(mockRequestingAgencyReportRow.getNonSubmissionReason()).thenReturn(LawEnforcementDetails.NonSubmissionReason.NOT_A_CRIME);
    when(mockRequestingAgencyReportRow.getProsecutorNotes()).thenReturn("Prosecutor Notes");
    when(mockRequestingAgencyReportRow.getLabReceivedDates()).thenReturn(Arrays.asList(LocalDate.of(2017, Month.FEBRUARY, 1)));
    when(mockRequestingAgencyReportRow.getDnaDatabaseEntryDate()).thenReturn(LocalDate.of(2017, Month.FEBRUARY, 1));
    when(mockRequestingAgencyReportRow.getDnaDatabaseHitDate()).thenReturn(LocalDate.of(2017, Month.FEBRUARY, 1));

    when(mockCurrentAssignmentReport.getGroups()).thenReturn(Arrays.asList(mockCurrentAssignmentReportGroup));
    when(mockCurrentAssignmentReportGroup.getRows()).thenReturn(Arrays.asList(mockCurrentAssignmentReportRow));
    when(mockCurrentAssignmentReportRow.getName()).thenReturn("Name");
    when(mockCurrentAssignmentReportRow.getType()).thenReturn(OrganizationType.MEDICAL);
    when(mockCurrentAssignmentReportRow.getSerialNumber()).thenReturn("012345");
    when(mockCurrentAssignmentReportRow.getCreatedDate()).thenReturn(LocalDate.of(2017, Month.JUNE, 1));
    when(mockCurrentAssignmentReportRow.getExpirationDate()).thenReturn(LocalDate.of(2017, Month.AUGUST, 1));

    when(mockStatutoryRequirementReport.getGroups()).thenReturn(Arrays.asList(mockStatutoryRequirementReportGroup));
    when(mockStatutoryRequirementReportGroup.getRows()).thenReturn(Arrays.asList(mockStatutoryRequirementReportRow));
    when(mockStatutoryRequirementReportGroup.getRequirement()).thenReturn("Requirement");
    when(mockStatutoryRequirementReportRow.getName()).thenReturn("Name");
    when(mockStatutoryRequirementReportRow.getSerialNumber()).thenReturn("012345");
    when(mockStatutoryRequirementReportRow.getLeCaseNumber()).thenReturn("555");
    when(mockStatutoryRequirementReportRow.getLabCaseNumber()).thenReturn("M2017-123");
    when(mockStatutoryRequirementReportRow.getCollectionDate()).thenReturn(LocalDate.of(2017, Month.JANUARY, 1));
    when(mockStatutoryRequirementReportRow.getDays()).thenReturn(Long.valueOf(40));
  }

  @Test
  public void convertLifeCycleReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,Days at Medical,Currently at Medical,"
      + "Days in Transit,Currently in Transit,Days at Law Enforcement,Currently at Law Enforcement,"
      + "Days in Analysis,Currently in Analysis";

    String expectedRow = "\"Name\",\"012345\",\"1\",false,\"2\",false,\"3\",false,\"4\",true";
    String expectedFileName = "life-cycle-report.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportLifeCycleReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertUnusedKitsReport() {
    String expectedHeader = "Agency,Type,Serial Number,Created Date,Expiration Date";
    String expectedRow = "\"Name\",\"Medical\",\"012345\",\"06/01/2017\",\"08/01/2017\"";
    String expectedFileName = "unused-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportUnusedKitsReport(mockCurrentAssignmentReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertUnreceivedCollectedKitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,Collection Date,Last Modified By,Last Modified Date";
    String expectedRow = "\"Name\",\"012345\",\"01/01/2017\",\"Modifier\",\"02/16/2017\"";
    String expectedFileName = "unreceived-collected-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportUnreceivedCollectedKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertReceivedCollectedKitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,Collection Date,Days at Medical";
    String expectedRow = "\"Name\",\"012345\",\"01/01/2017\",\"1\"";
    String expectedFileName = "received-collected-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportReceivedCollectedKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertExceedingStatutoryRequirementsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,LE Case Number,Lab Case Number,Collection Date,Number of Days,Requirement";
    String expectedRow = "\"Name\",\"012345\",\"555\",\"M2017-123\",\"01/01/2017\",\"40\",\"Requirement\"";
    String expectedFileName = "exceeding-statutory-requirements.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportExceedingStatutoryRequirementsReport(mockStatutoryRequirementReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertUnsubmittableKitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,Reason,Prosecutor Notes";
    String expectedRow = "\"Name\",\"012345\",\"It is no longer being investigated as a crime.\",\"Prosecutor Notes\"";
    String expectedFileName = "unsubmittable-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportUnsubmittableKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertSubmittedKitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,Date Received at Lab";
    String expectedRow = "\"Name\",\"012345\",\"02/01/2017\"";
    String expectedFileName = "submitted-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportSubmittedKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertDnaDatabaseKitsWithoutHitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,DNA Database Entry Date";
    String expectedRow = "\"Name\",\"012345\",\"02/01/2017\"";
    String expectedFileName = "dna-database-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportDatabaseKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertDnaDatabaseKitsWithHitsReport() {
    String expectedHeader = "Law Enforcement Agency,Serial Number,DNA Database Entry Date,Hit Date";
    String expectedRow = "\"Name\",\"012345\",\"02/01/2017\",\"02/01/2017\"";
    String expectedFileName = "dna-database-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportDatabaseHitKitsReport(mockRequestingAgencyReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

  @Test
  public void convertKitsWithQuestionableEventsReport() {
    String expectedHeader = "Agency,Serial Number";
    String expectedRow = "\"Name\",\"012345\"";
    String expectedFileName = "questionable-events-kits.csv";

    String expectedContent = expectedHeader + NEWLINE + expectedRow;

    CsvDownloadable d = exporter.exportKitsWithQuestionableEventsReport(mockCurrentAssignmentReport);
    Assert.assertEquals(expectedFileName, d.getFileName());
    Assert.assertArrayEquals(expectedContent.getBytes(), d.getContent());
    Assert.assertEquals(expectedContent.getBytes().length, d.getLength());
  }

}
