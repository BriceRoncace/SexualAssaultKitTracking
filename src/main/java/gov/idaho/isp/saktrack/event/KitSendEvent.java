package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;

public class KitSendEvent extends KitTransitionEvent {
  public KitSendEvent(OrganizationUser user, EventDetails eventDetails) {
    super(user, eventDetails);
  }
}
