package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.time.LocalDate;

public class StatutoryRequirementReportRow {
  private final SexualAssaultKit kit;
  private final StatutoryRequirementType type;

  public StatutoryRequirementReportRow(SexualAssaultKit kit, StatutoryRequirementType type) {
    this.kit = kit;
    this.type = type;
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

  public String getLeCaseNumber() {
    return kit.getLeDetails() != null ? kit.getLeDetails().getCaseNumber() : null;
  }

  public String getLabCaseNumber() {
    return kit.getLabDetails() != null ? kit.getLabDetails().getCaseNumber() : null;
  }

  public LocalDate getCollectionDate() {
    return kit.getMedicalDetails() != null ? kit.getMedicalDetails().getCollectionDate() : null;
  }

  public Long getDays() {
    return type.getDays(kit);
  }
}
