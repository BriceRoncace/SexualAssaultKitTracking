package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import java.time.LocalDate;
import java.util.Optional;

public class CurrentAssignmentReportRow {
  private final SexualAssaultKit kit;

  public CurrentAssignmentReportRow(SexualAssaultKit kit) {
    this.kit = kit;
  }

  public String getName() {
    if (kit.getCurrentAssignment() != null) {
      return kit.getCurrentAssignment().getName();
    }
    return null;
  }

  public OrganizationType getType() {
    if (kit.getCurrentAssignment() != null) {
      return kit.getCurrentAssignment().getType();
    }
    return null;
  }

  public Long getId() {
    return kit.getId();
  }

  public String getSerialNumber() {
    return kit.getSerialNumber();
  }

  public LocalDate getExpirationDate() {
    return kit.getExpirationDate();
  }

  public LocalDate getCreatedDate() {
    return findEventDate(kit, ChainOfCustodyEvent.EventType.CREATE);
  }

  private LocalDate findEventDate(SexualAssaultKit kit, ChainOfCustodyEvent.EventType eventType) {
    Optional<ChainOfCustodyEvent> event = kit.getChainOfCustody().stream().filter(e -> eventType.equals(e.getEventType())).findFirst();
    return event.isPresent() ? event.get().getEventDate() : null;
  }
}