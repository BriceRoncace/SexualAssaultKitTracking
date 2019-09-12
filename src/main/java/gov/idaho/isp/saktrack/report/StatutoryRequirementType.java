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
import gov.idaho.isp.saktrack.util.MilestoneUtils;

public enum StatutoryRequirementType {
  TRANSPORT("Unreceived by Law Enforcement") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getMedicalSubmitDate(kit).isPresent() && !MilestoneUtils.getLeReceiveDate(kit).isPresent() && MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null) >= 10;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null);
    }
  },
  LE("At Law Enforcement") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getLeReceiveDate(kit).isPresent() && !MilestoneUtils.getProsecutorAgreedDateOrSentToLabDate(kit).isPresent() && MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null) >= 30;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null);
    }
  },
  LAB("At Lab") {
    @Override
    boolean filter(SexualAssaultKit kit) {
      return MilestoneUtils.getLabReceiveDate(kit).isPresent() && !MilestoneUtils.getLabCompleteDate(kit).isPresent() && MilestoneUtils.getTimeAtLab(kit).orElse(null) >= 90;
    }

    @Override
    Long getDays(SexualAssaultKit kit) {
      return MilestoneUtils.getTimeAtLab(kit).orElse(null);
    }
  };

  abstract boolean filter(SexualAssaultKit kit);
  abstract Long getDays(SexualAssaultKit kit);
  private final String label;

  private StatutoryRequirementType(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }
}
