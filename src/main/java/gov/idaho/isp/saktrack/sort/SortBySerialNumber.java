package gov.idaho.isp.saktrack.sort;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import java.util.Comparator;
import org.apache.commons.lang3.StringUtils;

public class SortBySerialNumber implements Comparator<SexualAssaultKit> {
  @Override
  public int compare(SexualAssaultKit kit1, SexualAssaultKit kit2) {
    if (StringUtils.isNumeric(kit1.getSerialNumber()) && StringUtils.isNumeric(kit2.getSerialNumber())) {
      Long n1 = Long.parseLong(kit1.getSerialNumber());
      Long n2 = Long.parseLong(kit2.getSerialNumber());
      return n1.compareTo(n2);
    }
    return kit1.getSerialNumber().compareTo(kit2.getSerialNumber());
  }
}
