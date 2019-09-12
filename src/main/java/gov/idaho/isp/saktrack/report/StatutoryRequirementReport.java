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

package gov.idaho.isp.saktrack.report;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class StatutoryRequirementReport {
  private Integer kitSize;
  private List<StatutoryRequirementReportGroup> groups = new ArrayList<>();

  public StatutoryRequirementReport(List<SexualAssaultKit> kits) {
    kitSize = kits.size();
    List<SexualAssaultKit> filteredKits = kits.stream().filter(k -> k.getCurrentAssignment() != null).collect(Collectors.toList());
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.TRANSPORT.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.TRANSPORT));
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.LE.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.LE));
    groups.add(new StatutoryRequirementReportGroup(filteredKits.stream().filter(k -> StatutoryRequirementType.LAB.filter(k)).collect(Collectors.toList()), StatutoryRequirementType.LAB));
  }

  public Integer getKitSize() {
    return kitSize;
  }

  public void setKitSize(Integer kitSize) {
    this.kitSize = kitSize;
  }

  public List<StatutoryRequirementReportGroup> getGroups() {
    return groups;
  }

  public void setGroups(List<StatutoryRequirementReportGroup> groups) {
    this.groups = groups;
  }
}
