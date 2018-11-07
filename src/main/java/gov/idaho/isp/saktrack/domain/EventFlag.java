package gov.idaho.isp.saktrack.domain;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.util.UserUtils;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.util.Optional;

public enum EventFlag implements HasLabel {
  NEW_KIT_SENT_FROM_LAB("Lab sent uncollected kit", true),
  MEDICAL_SENT_TO_LE("Medical sent collected kit to law enforcement", true),
  LE_RECEIVED_COLLECTED_KIT("Requesting law enforcement agency received collected kit", false),
  LE_SENT_FOR_ANALYSIS("Requesting law enforcement agency sent kit for analysis", true),
  LAB_RECEIVED("Lab received collected kit for analysis", false),
  LE_RECEIVED_ANALYZED_KIT("Requesting law enforcement agency received analyzed kit", false);

  private final String label;
  private final boolean sendEvent;

  private EventFlag(String label, boolean isSendEvent) {
    this.label = label;
    this.sendEvent = isSendEvent;
  }

  public boolean isSendEvent() {
    return sendEvent;
  }

  @Override
  public String getLabel() {
    return label;
  }

  public static Optional<EventFlag> flagSend(OrganizationUser sender, SexualAssaultKit kit, Organization sendTo) {
    if (UserUtils.isLabUser(sender) && isMedical(sendTo) && KitStatus.UNUSED.equals(kit.getStatus())) {
      return Optional.of(NEW_KIT_SENT_FROM_LAB);
    }
    if (UserUtils.isMedicalUser(sender) && isLawEnforcement(sendTo) && MilestoneUtils.getMedicalCollectedDate(kit).isPresent()) {
      return Optional.of(MEDICAL_SENT_TO_LE);
    }
    if (UserUtils.isLawEnforcementUser(sender) && isUserFromRequestingLeAgency(sender, kit) && kit.getStatus() == KitStatus.READY_TO_SEND_FOR_ANALYSIS) {
      return Optional.of(LE_SENT_FOR_ANALYSIS);
    }
    return Optional.empty();
  }

  public static Optional<EventFlag> flagReceive(OrganizationUser receiver, SexualAssaultKit kit, Organization sentFrom) {
    if (UserUtils.isLawEnforcementUser(receiver) && KitStatus.ANALYZED.equals(kit.getStatus()) && isUserFromRequestingLeAgency(receiver, kit)) {
      return Optional.of(LE_RECEIVED_ANALYZED_KIT);
    }
    if (UserUtils.isLawEnforcementUser(receiver) && MilestoneUtils.getMedicalCollectedDate(kit).isPresent() && isUserFromRequestingLeAgency(receiver, kit)) {
      return Optional.of(LE_RECEIVED_COLLECTED_KIT);
    }
    if (UserUtils.isLabUser(receiver) && isLawEnforcement(sentFrom) && MilestoneUtils.getMedicalCollectedDate(kit).isPresent()) {
      return Optional.of(LAB_RECEIVED);
    }

    return Optional.empty();
  }

  private static boolean isMedical(Organization org) {
    return org.getType() == OrganizationType.MEDICAL;
  }

  private static boolean isLawEnforcement(Organization org) {
    return org.getType() == OrganizationType.LAW_ENFORCEMENT;
  }

  private static boolean isLab(Organization org) {
    return org.getType() == OrganizationType.LAB;
  }

  private static boolean isUserFromRequestingLeAgency(OrganizationUser user, SexualAssaultKit kit) {
    return kit.getMedicalDetails().getRequestingLeAgency() != null
      && kit.getMedicalDetails().getRequestingLeAgency().getId() != null
      && kit.getMedicalDetails().getRequestingLeAgency().getId().equals(user.getOrganization().getId());
  }
}