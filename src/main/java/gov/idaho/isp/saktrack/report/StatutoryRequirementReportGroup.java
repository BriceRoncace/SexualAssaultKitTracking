package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

public class StatutoryRequirementReportGroup {
  private String requirement;
  private List<StatutoryRequirementReportRow> rows;

  public StatutoryRequirementReportGroup(List<SexualAssaultKit> kits, StatutoryRequirementType type) {
    this.requirement = type.getLabel();
    this.rows = kits.stream().map(k -> new StatutoryRequirementReportRow(k, type)).collect(Collectors.toList());
  }

  public String getGroupId() {
    return requirement.replaceAll("[^a-zA-Z0-9]+", "");
  }

  public String getRequirement() {
    return requirement;
  }

  public void setRequirement(String requirement) {
    this.requirement = requirement;
  }

  public List<StatutoryRequirementReportRow> getRows() {
    return rows;
  }

  public void setRows(List<StatutoryRequirementReportRow> rows) {
    this.rows = rows;
  }

  public BigDecimal getAverageDays() {
    List<Long> days = rows.stream().filter(r -> r.getDays() != null).map(StatutoryRequirementReportRow::getDays).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }
}
