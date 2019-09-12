/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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