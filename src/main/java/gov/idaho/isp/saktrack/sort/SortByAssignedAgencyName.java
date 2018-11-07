package gov.idaho.isp.saktrack.sort;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.util.Comparator;

public class SortByAssignedAgencyName implements Comparator<SexualAssaultKit> {
  @Override
  public int compare(SexualAssaultKit kit1, SexualAssaultKit kit2) {
    String name1 = (kit1.getCurrentAssignment() != null) ? kit1.getCurrentAssignment().getName() : "";
    String name2 = (kit2.getCurrentAssignment() != null) ? kit2.getCurrentAssignment().getName() : "";
    return name1.compareTo(name2);
  }

}
