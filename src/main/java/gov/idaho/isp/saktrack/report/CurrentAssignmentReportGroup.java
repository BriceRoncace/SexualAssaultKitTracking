package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.util.List;
import java.util.stream.Collectors;

public class CurrentAssignmentReportGroup {
  private String agencyName;
  private List<CurrentAssignmentReportRow> rows;

  public CurrentAssignmentReportGroup(String agencyName, List<SexualAssaultKit> kits) {
    this.agencyName = agencyName;
    this.rows = kits.stream().map(k -> new CurrentAssignmentReportRow(k)).collect(Collectors.toList());
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

  public List<CurrentAssignmentReportRow> getRows() {
    return rows;
  }

  public void setRows(List<CurrentAssignmentReportRow> rows) {
    this.rows = rows;
  }
}
