package gov.idaho.isp.saktrack.eventlistener;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.KitStatus;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.event.KitSendEvent;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;
import gov.idaho.isp.saktrack.persistence.SexualAssaultKitRepository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
public class KitSentToLabListener {
  private final SexualAssaultKitRepository sexualAssaultKitRepository;

  public KitSentToLabListener(SexualAssaultKitRepository sexualAssaultKitRepository) {
    this.sexualAssaultKitRepository = sexualAssaultKitRepository;
  }

  @EventListener
  public void defaultLabRequestingLeAgencyToLeSender(KitSendEvent e) {
    if (StringUtils.isNotBlank(e.getEventDetails().getSerialNumber())) {
      SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(e.getEventDetails().getSerialNumber());
      if (kit != null && kitNeedingAnalysisWasSentToLabFromLe(kit) && !hasLabRequestingLeAgency(kit)) {
        kit.getLabDetails().setRequestingLeAgency(kit.getCurrentCustody().getFrom());
        sexualAssaultKitRepository.save(kit);
      }
    }
  }

  private boolean kitNeedingAnalysisWasSentToLabFromLe(SexualAssaultKit kit) {
    ChainOfCustodyEvent event = kit.getCurrentCustody();
    return kit.getStatus() == KitStatus.SENT_TO_BE_ANALYZED && isLawEnforcement(event.getFrom()) && isLab(event.getTo());
  }

  private static boolean isLawEnforcement(Organization org) {
    return org.getType() == OrganizationType.LAW_ENFORCEMENT;
  }

  private static boolean isLab(Organization org) {
    return org.getType() == OrganizationType.LAB;
  }

  private boolean hasLabRequestingLeAgency(SexualAssaultKit kit) {
    return kit.getLabDetails().getRequestingLeAgency() != null;
  }
}
