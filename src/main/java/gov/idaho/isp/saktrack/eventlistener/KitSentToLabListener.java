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

package gov.idaho.isp.saktrack.eventlistener;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.SexualAssaultKitRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.event.KitSendEvent;
import java.util.Collections;
import java.util.List;
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
    for (String serialNumber : getSerialNumbers(e)) {
      if (StringUtils.isNotBlank(serialNumber)) {
        SexualAssaultKit kit = sexualAssaultKitRepository.findBySerialNumber(serialNumber);
        if (kit != null && kitNeedingAnalysisWasSentToLabFromLe(kit) && !hasLabRequestingLeAgency(kit)) {
          kit.getLabDetails().setRequestingLeAgency(kit.getCurrentCustody().getFrom());
          sexualAssaultKitRepository.save(kit);
        }
      }
    }
  }

  private List<String> getSerialNumbers(KitSendEvent e) {
    return e != null && e.getEventDetails() != null && e.getEventDetails().getSerialNumberList() != null ? e.getEventDetails().getSerialNumberList() : Collections.emptyList();
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
