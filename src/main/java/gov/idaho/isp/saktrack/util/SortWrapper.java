package gov.idaho.isp.saktrack.util;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Sort;

public class SortWrapper {
  private final Sort sort;

  private SortWrapper(Sort sort) {
    this.sort = sort;
  }

  public static SortWrapper valueOf(String s) {
    if (StringUtils.isBlank(s)) {
      return null;
    }
    return new SortWrapper(parseAsSort(s));
  }

  private static Sort parseAsSort(String s) {
    String[] propAndDir = s.split(",");

    String prop = propAndDir[0];
    String dir = (propAndDir.length > 1) ? propAndDir[1] : "ASC";

    return Sort.by(Sort.Direction.valueOf(dir), prop);
  }

  public Sort unwrap() {
    return sort;
  }

  @Override
  public String toString() {
    return "SortWrapper{" + "sort=" + sort + '}';
  }
}
