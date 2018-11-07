package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;

public class KitCreateEvent extends KitTransitionEvent {
  public KitCreateEvent(OrganizationUser user, EventDetails eventDetails) {
    super(user, eventDetails);
  }
}
