package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;

public class KitReceiveEvent extends KitTransitionEvent {
  public KitReceiveEvent(OrganizationUser user, EventDetails eventDetails) {
    super(user, eventDetails);
  }
}
