package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.EventFlag;
import gov.idaho.isp.saktrack.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public class RequestingAgencyReportRow {
  private final SexualAssaultKit kit;

  public RequestingAgencyReportRow(SexualAssaultKit kit) {
    this.kit = kit;
  }

  public String getName() {
    if (kit.getMedicalDetails() != null && kit.getMedicalDetails().getRequestingLeAgency() != null) {
      return kit.getMedicalDetails().getRequestingLeAgency().getName();
    }
    return null;
  }

  public Long getId() {
    return kit.getId();
  }

  public String getSerialNumber() {
    return kit.getSerialNumber();
  }

  public Long getTimeAtMedical() {
    return MilestoneUtils.getTimeAtMedical(kit).orElse(null);
  }

  public Long getTimeInTransit() {
    return MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null);
  }

  public Long getTimeAtLawEnforcement() {
    return MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null);
  }

  public Long getTimeAtLab() {
    return MilestoneUtils.getTimeAtLab(kit).orElse(null);
  }

  public String getLastModifiedBy() {
    return kit.getLastModifiedBy();
  }

  public LocalDateTime getLastModified() {
    return kit.getLastModified();
  }

  public boolean isAtMedical() {
    return MilestoneUtils.getMedicalCollectedDate(kit).isPresent() && !MilestoneUtils.getMedicalSubmitDate(kit).isPresent();
  }

  public boolean isInTransit() {
    return MilestoneUtils.getMedicalSubmitDate(kit).isPresent() && !MilestoneUtils.getLeReceiveDate(kit).isPresent();
  }

  public boolean isAtLawEnforcement() {
    return MilestoneUtils.getLeReceiveDate(kit).isPresent() && !MilestoneUtils.getProsecutorAgreedDateOrSentToLabDate(kit).isPresent();
  }

  public boolean isAtLab() {
    return MilestoneUtils.getLabReceiveDate(kit).isPresent() && !MilestoneUtils.getLabCompleteDate(kit).isPresent();
  }

  public List<LocalDate> getLabReceivedDates() {
    return MilestoneUtils.getAllDatesForEventFlag(kit, EventFlag.LAB_RECEIVED);
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return kit.getLeDetails().getNonSubmissionReason();
  }

  public String getProsecutorNotes() {
    if (kit.getLeDetails() != null && kit.getLegalDetails().getProsecutorNotes() != null) {
      return kit.getLegalDetails().getProsecutorNotes();
    }
    return "";
  }

  public List<ChainOfCustodyEvent> getChainOfCustody() {
    return kit.getChainOfCustody();
  }

  public LocalDate getDnaDatabaseEntryDate() {
    return kit.getLabDetails().getDnaDatabaseEntryDate();
  }

  public LocalDate getDnaDatabaseHitDate() {
    return kit.getLabDetails().getDnaDatabaseHitDate();
  }

  public LocalDate getCollectionDate() {
    return kit.getMedicalDetails().getCollectionDate();
  }

}
