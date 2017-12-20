package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;

public class KitDestroyEvent extends KitTransitionEvent {
  public KitDestroyEvent(OrganizationUser user, EventDetails eventDetails) {
    super(user, eventDetails);
  }
}
