package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class RequestingAgencyReport {
  private Integer kitSize;
  private List<RequestingAgencyReportGroup> groups = new ArrayList<>();

  public RequestingAgencyReport(List<SexualAssaultKit> kits) {
    kitSize = kits.size();
    List<SexualAssaultKit> filteredKits = kits.stream().filter(k -> k.getMedicalDetails() != null && k.getMedicalDetails().getRequestingLeAgency() != null).collect(Collectors.toList());
    List<String> names = filteredKits.stream().map(k -> k.getMedicalDetails().getRequestingLeAgency().getName()).distinct().collect(Collectors.toList());
    names.forEach(name -> {
      groups.add(new RequestingAgencyReportGroup(name, filteredKits.stream().filter(k -> name.equals(k.getMedicalDetails().getRequestingLeAgency().getName())).collect(Collectors.toList())));
    });
  }

  public BigDecimal getTimeAtMedicalAverage() {
    List<Long> days = groups.stream().flatMap(g -> g.getRows().stream()).filter(k -> k.getTimeAtMedical() != null).map(RequestingAgencyReportRow::getTimeAtMedical).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeInTransitAverage() {
    List<Long> days = groups.stream().flatMap(g -> g.getRows().stream()).filter(k -> k.getTimeInTransit() != null).map(RequestingAgencyReportRow::getTimeInTransit).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeAtLawEnforcementAverage() {
    List<Long> days = groups.stream().flatMap(g -> g.getRows().stream()).filter(k -> k.getTimeAtLawEnforcement() != null).map(RequestingAgencyReportRow::getTimeAtLawEnforcement).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeAtLabAverage() {
    List<Long> days = groups.stream().flatMap(g -> g.getRows().stream()).filter(k -> k.getTimeAtLab() != null).map(RequestingAgencyReportRow::getTimeAtLab).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public Integer getKitSize() {
    return kitSize;
  }

  public void setKitSize(Integer kitSize) {
    this.kitSize = kitSize;
  }

  public List<RequestingAgencyReportGroup> getGroups() {
    return groups;
  }

  public void setGroups(List<RequestingAgencyReportGroup> groups) {
    this.groups = groups;
  }

}
