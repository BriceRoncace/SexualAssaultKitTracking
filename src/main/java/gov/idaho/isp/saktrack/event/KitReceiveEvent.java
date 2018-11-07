package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;

public class KitReceiveEvent extends KitTransitionEvent {
  public KitReceiveEvent(OrganizationUser user, EventDetails eventDetails) {
    super(user, eventDetails);
  }
}
