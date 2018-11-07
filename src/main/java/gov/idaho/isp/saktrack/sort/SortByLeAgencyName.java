package gov.idaho.isp.saktrack.sort;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.util.Comparator;

public class SortByLeAgencyName implements Comparator<SexualAssaultKit> {
  @Override
  public int compare(SexualAssaultKit kit1, SexualAssaultKit kit2) {
    String name1 = (kit1.getMedicalDetails() != null && kit1.getMedicalDetails().getRequestingLeAgency() != null) ? kit1.getMedicalDetails().getRequestingLeAgency().getName() : "";
    String name2 = (kit2.getMedicalDetails() != null && kit2.getMedicalDetails().getRequestingLeAgency() != null) ? kit2.getMedicalDetails().getRequestingLeAgency().getName() : "";
    return name1.compareTo(name2);
  }
}
