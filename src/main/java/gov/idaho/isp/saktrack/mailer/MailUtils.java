package gov.idaho.isp.saktrack.mailer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

public class MailUtils {
  public static List<String> splitAddresses(String addresses) {
    if (addresses == null) {
      return Collections.emptyList();
    }

    return addresses.contains(";") ? Arrays.asList(addresses.split(";")) : Arrays.asList(addresses);
  }

  public static Collection<String> clean(Collection<String> addresses) {
    removeEmptyValues(addresses);
    addresses = MailUtils.trimValues(addresses);
    addresses = MailUtils.removeDuplicates(addresses);
    return addresses;
  }

  private static Collection<String> trimValues(Collection<String> strings) {
    if (strings != null) {
      return strings.stream().map(String::trim).collect(Collectors.toList());
    }
    return strings;
  }

  private static void removeEmptyValues(Collection<String> strings) {
    if (strings != null) {
      strings.removeIf(s -> s == null || "".equals(s.trim()));
    }
  }

  private static <T> List<T> removeDuplicates(Collection<T> list) {
    return new ArrayList<>(new LinkedHashSet<>(list));
  }
}