package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class StatutoryRequirementReport {
  private Integer kitSize;
  private List<StatutoryRequirementReportGroup> groups = new ArrayList<>();

  public StatutoryRequirementReport(List<SexualAssaultKit> kits) {
    kitSize = kits.size();
    List<SexualAssaultKit> filteredKits = kits.stream().filter(k -> k.getCurrentAssignment() != null).collect(Collectors.toList());
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.TRANSPORT.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.TRANSPORT));
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.LE.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.LE));
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.LAB.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.LAB));
  }

  public Integer getKitSize() {
    return kitSize;
  }

  public void setKitSize(Integer kitSize) {
    this.kitSize = kitSize;
  }

  public List<StatutoryRequirementReportGroup> getGroups() {
    return groups;
  }

  public void setGroups(List<StatutoryRequirementReportGroup> groups) {
    this.groups = groups;
  }
}
