package gov.idaho.isp.saktrack.config;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.EventFlag;
import gov.idaho.isp.saktrack.organization.Organization;
import java.time.LocalDate;

public class EventBuilder {
  private ChainOfCustodyEvent.EventType eventType;
  private LocalDate eventDate;
  private EventFlag eventFlag;
  private Organization to;
  private Organization from;

  public EventBuilder eventType(ChainOfCustodyEvent.EventType eventType) {
    this.eventType = eventType;
    return this;
  }

  public EventBuilder eventDate(LocalDate eventDate) {
    this.eventDate = eventDate;
    return this;
  }

  public EventBuilder eventFlag(EventFlag eventFlag) {
    this.eventFlag = eventFlag;
    return this;
  }

  public EventBuilder to(Organization to) {
    this.to = to;
    return this;
  }

  public EventBuilder from(Organization from) {
    this.from = from;
    return this;
  }

  public ChainOfCustodyEvent build() {
    ChainOfCustodyEvent event = new ChainOfCustodyEvent();
    event.setEventDate(eventDate);
    event.setEventType(eventType);
    event.setEventFlag(eventFlag);
    event.setTo(to);
    event.setFrom(from);
    return event;
  }
}
