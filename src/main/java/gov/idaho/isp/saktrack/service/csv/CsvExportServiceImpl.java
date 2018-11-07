package gov.idaho.isp.saktrack.service.csv;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
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
import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Service
public class CsvExportServiceImpl implements CsvExportService {
  private final String EMPTY_STR = "";
  private final String QUOTE = "\"";
  private final String DOUBLE_QUOTE = "\"\"";
  private final String COMMA = ",";
  private final String NEW_LINE = "\n";

  private final Pattern SINGLEQUOTE = Pattern.compile(QUOTE);

  private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy");


  @Override
  public CsvDownloadable exportLifeCycleReport(RequestingAgencyReport report) {
    String header = "Law Enforcement Agency,Serial Number,Days at Medical,Currently at Medical,"
      + "Days in Transit,Currently in Transit,Days at Law Enforcement,Currently at Law Enforcement,"
      + "Days in Analysis,Currently in Analysis";

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getTimeAtMedical()));
        row.add(cleanString(r.isAtMedical()));
        row.add(cleanString(r.getTimeInTransit()));
        row.add(cleanString(r.isInTransit()));
        row.add(cleanString(r.getTimeAtLawEnforcement()));
        row.add(cleanString(r.isAtLawEnforcement()));
        row.add(cleanString(r.getTimeAtLab()));
        row.add(cleanString(r.isAtLab()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("life-cycle-report.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportUnusedKitsReport(CurrentAssignmentReport report) {
    String header = "Agency,Type,Serial Number,Created Date,Expiration Date";

    List<String> reportValues = new ArrayList<>();

    for (CurrentAssignmentReportGroup g : report.getGroups()) {
      for (CurrentAssignmentReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getType() != null ? r.getType().getLabel() : null));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getCreatedDate()));
        row.add(cleanString(r.getExpirationDate()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("unused-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportUnreceivedCollectedKitsReport(RequestingAgencyReport report) {
    String header = "Law Enforcement Agency,Serial Number,Collection Date,Last Modified By,Last Modified Date";

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getCollectionDate()));
        row.add(cleanString(r.getLastModifiedBy()));
        row.add(cleanString(r.getLastModified()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("unreceived-collected-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportReceivedCollectedKitsReport(RequestingAgencyReport report) {
    String header = "Law Enforcement Agency,Serial Number,Collection Date,Days at Medical";

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getCollectionDate()));
        row.add(cleanString(r.getTimeAtMedical()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("received-collected-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportExceedingStatutoryRequirementsReport(StatutoryRequirementReport report) {
    String header = "Law Enforcement Agency,Serial Number,LE Case Number,Lab Case Number,Collection Date,Number of Days,Requirement";

    List<String> reportValues = new ArrayList<>();

    for (StatutoryRequirementReportGroup g : report.getGroups()) {
      for (StatutoryRequirementReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getLeCaseNumber()));
        row.add(cleanString(r.getLabCaseNumber()));
        row.add(cleanString(r.getCollectionDate()));
        row.add(cleanString(r.getDays()));
        row.add(cleanString(g.getRequirement()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("exceeding-statutory-requirements.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportUnsubmittableKitsReport(RequestingAgencyReport report) {
    String header = "Law Enforcement Agency,Serial Number,Reason,Prosecutor Notes";

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getNonSubmissionReason().getLabel()));
        row.add(cleanString(r.getProsecutorNotes()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("unsubmittable-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportSubmittedKitsReport(RequestingAgencyReport report) {
    String header = "Law Enforcement Agency,Serial Number,Date Received at Lab";

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getLabReceivedDates()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("submitted-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportDatabaseKitsReport(RequestingAgencyReport report) {
    return exportDnaDatabaseKitsReport(report, false);
  }

  @Override
  public CsvDownloadable exportDatabaseHitKitsReport(RequestingAgencyReport report) {
    return exportDnaDatabaseKitsReport(report, true);
  }


  private CsvDownloadable exportDnaDatabaseKitsReport(RequestingAgencyReport report, boolean includeHitDate) {
    String header = "Law Enforcement Agency,Serial Number,DNA Database Entry Date";
    if (includeHitDate) {
      header = header + ",Hit Date";
    }

    List<String> reportValues = new ArrayList<>();

    for (RequestingAgencyReportGroup g : report.getGroups()) {
      for (RequestingAgencyReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        row.add(cleanString(r.getDnaDatabaseEntryDate()));
        if (includeHitDate) {
          row.add(cleanString(r.getDnaDatabaseHitDate()));
        }
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("dna-database-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportKitsWithQuestionableEventsReport(CurrentAssignmentReport report) {
    String header = "Agency,Serial Number";

    List<String> reportValues = new ArrayList<>();

    for (CurrentAssignmentReportGroup g : report.getGroups()) {
      for (CurrentAssignmentReportRow r : g.getRows()) {
        StringJoiner row = new StringJoiner(COMMA);
        row.add(cleanString(r.getName()));
        row.add(cleanString(r.getSerialNumber()));
        reportValues.add(row.toString());
      }
    }

    return new CsvDownloadable("questionable-events-kits.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportUserList(List<AbstractUser> users) {
    String header = "Username,Display Name,User Type,Email,Phone,Organization,Organization Type,Org Admin,Org Contact,Enabled,Verified Date,Notified of Incoming Kit,Notified of Reviewed Kit,Notified of New User";

    List<String> reportValues = new ArrayList<>();

    for (AbstractUser u : users) {
      StringJoiner row = new StringJoiner(COMMA);
      row.add(cleanString(u.getUsername()));
      row.add(cleanString(u.getDisplayName()));
      row.add(cleanString(u.getType().getLabel()));
      row.add(cleanString(u.getEmail()));
      row.add(cleanString(u.getPhone()));

      if (!(u instanceof AbstractOrganizationUser)) {
        row.add(EMPTY_STR);
        row.add(EMPTY_STR);
        row.add(EMPTY_STR);
        row.add(EMPTY_STR);
        row.add(EMPTY_STR);
      }
      else {
        AbstractOrganizationUser orgUser = (AbstractOrganizationUser) u;
        row.add(cleanString(orgUser.getOrganization().getName()));
        row.add(cleanString(orgUser.getOrganization().getType().getLabel()));
        row.add(cleanString(orgUser.isOrganizationAdmin()));
        row.add(cleanString(orgUser.isOrganizationContact()));
        row.add(cleanString(orgUser.isEnabled()));
        row.add(cleanString(orgUser.getVerifiedDate()));
        row.add(cleanString(orgUser.getIncomingKitEmail()));
        row.add(orgUser instanceof LawEnforcementUser ? cleanString(((LawEnforcementUser)orgUser).getSendAttorneyReviewedEmail()) : EMPTY_STR);
        row.add(cleanString(orgUser.getSendUserEmail()));
      }

      reportValues.add(row.toString());
    }

    return new CsvDownloadable("users.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportKitSearchResults(List<SexualAssaultKit> kits) {
    String header = "Serial #,Current Organization,Repurposed,Collected,Analyzed,DNA Database Entry Date,DNA Database Hit Date,Expunged Date,Planned Destruction Date,Destruction Date,Last Modified,Last Modified By,Most Recent Event";
    String medicalHeader = ",Expiration Date,Requesting Law Enforcement Agency";
    String victimType = ",Victim Type";
    String leHeader = ",Case Number,Investigator,Crime,Crime Date,Outsourced Lab,Planned Destruction Notification Requested,Meets Submission Criteria";
    String legalHeader = ",Non-Submission Reason,Prosecutor Agreement,Reviewing Prosecutor,Prosecutor Notes,Released for Review,Review Finalized";
    String labHeader = ",Requesting Law Enforcement Agency,Case Number,DNA Database Entry";

    header = header + medicalHeader + victimType + leHeader + legalHeader + labHeader;

    List<String> reportValues = new ArrayList<>();

    for (SexualAssaultKit k : kits) {
      StringJoiner row = new StringJoiner(COMMA);

      addVisibleToAllDetailsToRow(row, k);

      addMedicalDetailsToRow(row, k);

      if (k.getMedicalDetails().getVictimType() != null) {
        row.add(cleanString(k.getMedicalDetails().getVictimType().getLabel()));
      }
      else {
        row.add("");
      }

      addLeDetailsToRow(row, k);

      addLegalDetailsToRow(row, k);

      addLabDetailsToRow(row, k);

      reportValues.add(row.toString());
    }

    return new CsvDownloadable("search-results.csv", convertToContent(header, reportValues));
  }

  @Override
  public CsvDownloadable exportKitSearchResults(List<SexualAssaultKit> kits, OrganizationType orgType) {
    String header = "Serial #,Current Organization,Repurposed,Collected,Analyzed,DNA Database Entry Date,DNA Database Hit Date,Expunged Date,Planned Destruction Date,Destruction Date,Last Modified,Last Modified By,Most Recent Event";
    String leHeader = ",Case Number,Investigator,Crime,Crime Date,Outsourced Lab,Planned Destruction Notification Requested,Meets Submission Criteria";
    String legalHeader = ",Non-Submission Reason,Prosecutor Agreement,Reviewing Prosecutor,Prosecutor Notes,Released for Review,Review Finalized";
    String labHeader = ",Requesting Law Enforcement Agency,Case Number,DNA Database Entry";

    if (OrganizationType.MEDICAL.equals(orgType) || OrganizationType.LAB.equals(orgType)) {
      header = header + ",Expiration Date";
    }
    if (OrganizationType.MEDICAL.equals(orgType) || OrganizationType.LEGAL.equals(orgType)) {
      header = header + ",Requesting Law Enforcement Agency";
    }
    if (!OrganizationType.LAB.equals(orgType)) {
      header = header + ",Victim Type";
    }
    if (OrganizationType.LAW_ENFORCEMENT.equals(orgType) || OrganizationType.LEGAL.equals(orgType)) {
      header = header + leHeader + legalHeader;
    }
    if (OrganizationType.LAB.equals(orgType)) {
      header = header + labHeader;
    }

    List<String> reportValues = new ArrayList<>();

    for (SexualAssaultKit k : kits) {
      StringJoiner row = new StringJoiner(COMMA);

      addVisibleToAllDetailsToRow(row, k);

      if (OrganizationType.MEDICAL.equals(orgType) || OrganizationType.LAB.equals(orgType)) {
        row.add(cleanString(k.getExpirationDate()));
      }

      if (OrganizationType.MEDICAL.equals(orgType) || OrganizationType.LEGAL.equals(orgType)) {
        if (k.getMedicalDetails().getRequestingLeAgency() != null) {
          row.add(cleanString(k.getMedicalDetails().getRequestingLeAgency().getName()));
        }
        else {
          row.add(EMPTY_STR);
        }
      }

      if (!OrganizationType.LAB.equals(orgType)) {
        if (k.getMedicalDetails().getVictimType() != null) {
          row.add(cleanString(k.getMedicalDetails().getVictimType().getLabel()));
        }
        else {
          row.add(EMPTY_STR);
        }
      }

      if (OrganizationType.LAW_ENFORCEMENT.equals(orgType) || OrganizationType.LEGAL.equals(orgType)) {
        addLeDetailsToRow(row, k);
        addLegalDetailsToRow(row, k);
      }


      if (OrganizationType.LAB.equals(orgType)) {
        addLabDetailsToRow(row, k);
      }

      reportValues.add(row.toString());
    }

    return new CsvDownloadable("search-results.csv", convertToContent(header, reportValues));
  }

  private void addVisibleToAllDetailsToRow(StringJoiner row, SexualAssaultKit k) {
    row.add(cleanString(k.getSerialNumber()));
    if (k.getCurrentAssignment() != null) {
      row.add(cleanString(k.getCurrentAssignment().getName()));
    }
    else {
      row.add(EMPTY_STR);
    }
    row.add(cleanString(findRepurosedDate(k)));
    row.add(cleanString(k.getMedicalDetails().getCollectionDate()));
    row.add(cleanString(k.getLabDetails().getDateCompleted()));
    row.add(cleanString(k.getLabDetails().getDnaDatabaseEntryDate()));
    row.add(cleanString(k.getLabDetails().getDnaDatabaseHitDate()));
    row.add(cleanString(k.getLabDetails().getExpungedDate()));
    row.add(cleanString(k.getLeDetails().getPlannedDestructionDate()));
    row.add(cleanString(findDestructionDate(k)));
    row.add(cleanString(k.getLastModified()));
    row.add(cleanString(k.getLastModifiedBy()));
    row.add(cleanString(k.getCurrentCustody().prettyPrint(false)));
  }

  private void addMedicalDetailsToRow(StringJoiner row, SexualAssaultKit k) {
    row.add(cleanString(k.getExpirationDate()));
    if (k.getMedicalDetails().getRequestingLeAgency() != null) {
      row.add(cleanString(k.getMedicalDetails().getRequestingLeAgency().getName()));
    }
    else {
      row.add(EMPTY_STR);
    }
  }

  private void addLeDetailsToRow(StringJoiner row, SexualAssaultKit k) {
    row.add(cleanString(k.getLeDetails().getCaseNumber()));
    row.add(cleanString(k.getLeDetails().getInvestigator()));
    row.add(cleanString(k.getLeDetails().getCrime()));
    row.add(cleanString(k.getLeDetails().getCrimeDate()));
    row.add(cleanString(k.getLeDetails().getOutsourcedLabName()));
    row.add(cleanString(k.getLeDetails().getPlannedDestructionNotificationRequested()));
    row.add(cleanString(k.getLeDetails().getMeetsSubmissionCriteria()));
  }

  private void addLegalDetailsToRow(StringJoiner row, SexualAssaultKit k) {
    if (k.getLegalDetails().getNonSubmissionReason() != null) {
      row.add(cleanString(k.getLegalDetails().getNonSubmissionReason().getLabel()));
    }
    else {
      row.add(EMPTY_STR);
    }
    row.add(cleanString(k.getLegalDetails().getProsecutorAgrees()));
    row.add(cleanString(k.getLegalDetails().getReviewingProsecutor()));
    row.add(cleanString(k.getLegalDetails().getProsecutorNotes()));
    row.add(cleanString(k.getLegalDetails().getReleasedForReview()));
    row.add(cleanString(k.getLegalDetails().getReviewFinalized()));
  }

  private void addLabDetailsToRow(StringJoiner row, SexualAssaultKit k) {
    if (k.getLabDetails().getRequestingLeAgency() != null) {
      row.add(cleanString(k.getLabDetails().getRequestingLeAgency().getName()));
    }
    else {
      row.add(EMPTY_STR);
    }
    row.add(cleanString(k.getLabDetails().getCaseNumber()));
    if (k.getLabDetails().getDnaDatabaseEntry() != null) {
      row.add(cleanString(k.getLabDetails().getDnaDatabaseEntry().getLabel()));
    }
    else {
      row.add(EMPTY_STR);
    }
  }

  private LocalDate findRepurosedDate(SexualAssaultKit kit) {
    return findEventDate(kit, ChainOfCustodyEvent.EventType.REPURPOSE);
  }

  private LocalDate findDestructionDate(SexualAssaultKit kit) {
    return findEventDate(kit, ChainOfCustodyEvent.EventType.DESTROY);
  }

  private LocalDate findEventDate(SexualAssaultKit kit, ChainOfCustodyEvent.EventType eventType) {
    Optional<ChainOfCustodyEvent> event = kit.getChainOfCustody().stream().filter(e -> eventType.equals(e.getEventType())).findFirst();
    return event.isPresent() ? event.get().getEventDate() : null;
  }

  private byte[] convertToContent(String header, List<String> rows) {
    List<String> exportData = new ArrayList<>();
    exportData.add(header);
    exportData.addAll(rows);
    String content = String.join(NEW_LINE, exportData);
    return content.getBytes();
  }

  private String cleanString(String str) {
    str = escapeQuotes(str);
    str = surroundWithQuotes(str);
    return str;
  }

  private String cleanString(boolean bool) {
    return String.valueOf(bool);
  }

  private String cleanString(Boolean bool) {
    return bool != null ? cleanString(bool.booleanValue()) : EMPTY_STR;
  }

  private String cleanString(Long l) {
    return l != null ? cleanString(String.valueOf(l)) : EMPTY_STR;
  }

  private String cleanString(LocalDate localDate) {
    return localDate != null ? cleanString(formatter.format(localDate)) : EMPTY_STR;
  }

  private String cleanString(LocalDateTime localDateTime) {
    return localDateTime != null ? cleanString(formatter.format(localDateTime)) : EMPTY_STR;
  }

  private String cleanString(List<LocalDate> localDates) {
    return cleanString(localDates.stream().map(ld -> formatter.format(ld)).collect(Collectors.joining(COMMA + " ")));
  }

  private String escapeQuotes(String str) {
    if (isEmpty(str)) {
      return EMPTY_STR;
    }
    return SINGLEQUOTE.matcher(str).replaceAll(DOUBLE_QUOTE).trim();
  }

  private String surroundWithQuotes(String str) {
    if (isEmpty(str)) {
      return EMPTY_STR;
    }
    return QUOTE + str +  QUOTE;
  }

  private boolean isEmpty(String str) {
    return StringUtils.stripToEmpty(str).isEmpty();
  }

}
