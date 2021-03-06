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
import java.util.List;
import java.util.stream.Collectors;

public class CurrentAssignmentReportGroup {
  private String agencyName;
  private List<CurrentAssignmentReportRow> rows;

  public CurrentAssignmentReportGroup(String agencyName, List<SexualAssaultKit> kits) {
    this.agencyName = agencyName;
    this.rows = kits.stream().map(k -> new CurrentAssignmentReportRow(k)).collect(Collectors.toList());
  }

  public String getGroupId() {
    return agencyName.replaceAll("[^a-zA-Z0-9]+", "");
  }

  public String getAgencyName() {
    return agencyName;
  }

  public void setAgencyName(String agencyName) {
    this.agencyName = agencyName;
  }

  public List<CurrentAssignmentReportRow> getRows() {
    return rows;
  }

  public void setRows(List<CurrentAssignmentReportRow> rows) {
    this.rows = rows;
  }
}
