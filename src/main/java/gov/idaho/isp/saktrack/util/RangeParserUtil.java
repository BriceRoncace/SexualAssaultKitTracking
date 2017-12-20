package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.service.RangeParser;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RangeParserUtil {
  private static RangeParser rangeParser;

  public static List<String> parse(String input) {
    return rangeParser.parse(input);
  }

  @Autowired
  public void setRangeParser(RangeParser rangeParser) {
    RangeParserUtil.rangeParser = rangeParser;
  }
}
