package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

public class RequestingAgencyReportGroup {
  private String agencyName;
  private List<RequestingAgencyReportRow> rows;

  public RequestingAgencyReportGroup(String agencyName, List<SexualAssaultKit> kits) {
    this.agencyName = agencyName;
    this.rows = kits.stream().map(k -> new RequestingAgencyReportRow(k)).collect(Collectors.toList());
  }

  public BigDecimal getTimeAtMedicalAverage() {
    List<Long> days = rows.stream().filter(k -> k.getTimeAtMedical() != null).map(RequestingAgencyReportRow::getTimeAtMedical).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeInTransitAverage() {
    List<Long> days = rows.stream().filter(k -> k.getTimeInTransit() != null).map(RequestingAgencyReportRow::getTimeInTransit).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeAtLawEnforcementAverage() {
    List<Long> days = rows.stream().filter(k -> k.getTimeAtLawEnforcement() != null).map(RequestingAgencyReportRow::getTimeAtLawEnforcement).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public BigDecimal getTimeAtLabAverage() {
    List<Long> days = rows.stream().filter(k -> k.getTimeAtLab() != null).map(RequestingAgencyReportRow::getTimeAtLab).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }

  public String getGroupId() {
    return agencyName.replaceAll("[^a-zA-Z0-9]+", "");
  }

  public String getAgencyName() {
    return agencyName;
  }

  public void setAgencyName(String agencyName) {
    this.agencyName = agencyName;
  }

  public List<RequestingAgencyReportRow> getRows() {
    return rows;
  }

  public void setRows(List<RequestingAgencyReportRow> rows) {
    this.rows = rows;
  }
}
