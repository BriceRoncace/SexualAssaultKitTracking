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
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

public class StatutoryRequirementReportGroup {
  private String requirement;
  private List<StatutoryRequirementReportRow> rows;

  public StatutoryRequirementReportGroup(List<SexualAssaultKit> kits, StatutoryRequirementType type) {
    this.requirement = type.getLabel();
    this.rows = kits.stream().map(k -> new StatutoryRequirementReportRow(k, type)).collect(Collectors.toList());
  }

  public String getGroupId() {
    return requirement.replaceAll("[^a-zA-Z0-9]+", "");
  }

  public String getRequirement() {
    return requirement;
  }

  public void setRequirement(String requirement) {
    this.requirement = requirement;
  }

  public List<StatutoryRequirementReportRow> getRows() {
    return rows;
  }

  public void setRows(List<StatutoryRequirementReportRow> rows) {
    this.rows = rows;
  }

  public BigDecimal getAverageDays() {
    List<Long> days = rows.stream().filter(r -> r.getDays() != null).map(StatutoryRequirementReportRow::getDays).collect(Collectors.toList());
    if (!days.isEmpty()) {
      return BigDecimal.valueOf(days.stream().mapToLong(l -> l).average().getAsDouble()).setScale(2, BigDecimal.ROUND_HALF_UP);
    }
    return null;
  }
}
