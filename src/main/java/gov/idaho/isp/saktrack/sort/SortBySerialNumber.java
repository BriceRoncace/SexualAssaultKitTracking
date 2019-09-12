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

package gov.idaho.isp.saktrack.sort;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
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
