package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.organization.Organization;

public class MissingSendEvent {
  private Organization from;
  private Organization to;

  public MissingSendEvent(Organization from, Organization to) {
    this.from = from;
    this.to = to;
  }

  public Organization getFrom() {
    return from;
  }

  public void setFrom(Organization from) {
    this.from = from;
  }

  public Organization getTo() {
    return to;
  }

  public void setTo(Organization to) {
    this.to = to;
  }
}
