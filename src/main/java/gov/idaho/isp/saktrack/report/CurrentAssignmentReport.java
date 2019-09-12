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

public class CurrentAssignmentReport {
  private Integer kitSize;
  private List<CurrentAssignmentReportGroup> groups = new ArrayList<>();

  public CurrentAssignmentReport(List<SexualAssaultKit> kits) {
    List<SexualAssaultKit> filteredKits = kits.stream().filter(k -> k.getCurrentAssignment() != null).collect(Collectors.toList());
    List<String> names = filteredKits.stream().map(k -> k.getCurrentAssignment().getName()).distinct().collect(Collectors.toList());
    names.forEach(name -> {
      groups.add(new CurrentAssignmentReportGroup(name, filteredKits.stream().filter(k -> name.equals(k.getCurrentAssignment().getName())).collect(Collectors.toList())));
    });
    kitSize = filteredKits.size();
  }

  public Integer getKitSize() {
    return kitSize;
  }

  public void setKitSize(Integer kitSize) {
    this.kitSize = kitSize;
  }

  public List<CurrentAssignmentReportGroup> getGroups() {
    return groups;
  }

  public void setGroups(List<CurrentAssignmentReportGroup> groups) {
    this.groups = groups;
  }
}
