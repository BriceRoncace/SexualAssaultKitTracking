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
import java.time.LocalDate;

public class StatutoryRequirementReportRow {
  private final SexualAssaultKit kit;
  private final StatutoryRequirementType type;

  public StatutoryRequirementReportRow(SexualAssaultKit kit, StatutoryRequirementType type) {
    this.kit = kit;
    this.type = type;
  }

  public String getName() {
    if (kit.getMedicalDetails() != null && kit.getMedicalDetails().getRequestingLeAgency() != null) {
      return kit.getMedicalDetails().getRequestingLeAgency().getName();
    }
    return null;
  }

  public Long getId() {
    return kit.getId();
  }

  public String getSerialNumber() {
    return kit.getSerialNumber();
  }

  public String getLeCaseNumber() {
    return kit.getLeDetails() != null ? kit.getLeDetails().getCaseNumber() : null;
  }

  public String getLabCaseNumber() {
    return kit.getLabDetails() != null ? kit.getLabDetails().getCaseNumber() : null;
  }

  public LocalDate getCollectionDate() {
    return kit.getMedicalDetails() != null ? kit.getMedicalDetails().getCollectionDate() : null;
  }

  public Long getDays() {
    return type.getDays(kit);
  }
}
