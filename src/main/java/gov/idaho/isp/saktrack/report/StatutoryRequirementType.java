package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.util.MilestoneUtils;

public enum StatutoryRequirementType {
  TRANSPORT("Unreceived by Law Enforcement") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getMedicalSubmitDate(kit).isPresent() && !MilestoneUtils.getLeReceiveDate(kit).isPresent() && MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null) >= 10;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null);
    }
  },
  LE("At Law Enforcement") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getLeReceiveDate(kit).isPresent() && !MilestoneUtils.getProsecutorAgreedDateOrSentToLabDate(kit).isPresent() && MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null) >= 30;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null);
    }
  },
  LAB("At Lab") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getLabReceiveDate(kit).isPresent() && !MilestoneUtils.getLabCompleteDate(kit).isPresent() && MilestoneUtils.getTimeAtLab(kit).orElse(null) >= 90;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeAtLab(kit).orElse(null);
    }
  };

  abstract boolean filter(SexualAssaultKit kit);
  abstract Long getDays(SexualAssaultKit kit);
  private final String label;

  private StatutoryRequirementType(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }
}
