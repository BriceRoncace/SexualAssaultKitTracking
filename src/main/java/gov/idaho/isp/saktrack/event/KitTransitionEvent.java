package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.time.LocalDateTime;

public abstract class KitTransitionEvent {
  private final OrganizationUser user;
  private final EventDetails eventDetails;
  private final LocalDateTime timestamp;

  public KitTransitionEvent(OrganizationUser user, EventDetails eventDetails) {
    this.user = user;
    this.eventDetails = eventDetails;
    this.timestamp = LocalDateTime.now();
  }

  public OrganizationUser getUser() {
    return user;
  }

  public EventDetails getEventDetails() {
    return eventDetails;
  }

  public LocalDateTime getTransitionEventTimestamp() {
    return timestamp;
  }
}
