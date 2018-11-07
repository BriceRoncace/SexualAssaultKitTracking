package gov.idaho.isp.saktrack.sort;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.time.LocalDate;
import java.util.Comparator;

public class SortByExpirationDate implements Comparator<SexualAssaultKit> {
  @Override
  public int compare(SexualAssaultKit kit1, SexualAssaultKit kit2) {
    LocalDate date1 = kit1.getExpirationDate();
    LocalDate date2 = kit2.getExpirationDate();
    return date1.compareTo(date2);
  }
}
