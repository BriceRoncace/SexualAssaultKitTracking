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

public class SortByLeAgencyName implements Comparator<SexualAssaultKit> {
  @Override
  public int compare(SexualAssaultKit kit1, SexualAssaultKit kit2) {
    String name1 = (kit1.getMedicalDetails() != null && kit1.getMedicalDetails().getRequestingLeAgency() != null) ? kit1.getMedicalDetails().getRequestingLeAgency().getName() : "";
    String name2 = (kit2.getMedicalDetails() != null && kit2.getMedicalDetails().getRequestingLeAgency() != null) ? kit2.getMedicalDetails().getRequestingLeAgency().getName() : "";
    return name1.compareTo(name2);
  }
}
