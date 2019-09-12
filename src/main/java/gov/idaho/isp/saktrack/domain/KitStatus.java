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

package gov.idaho.isp.saktrack.domain;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.time.LocalDate;
import org.apache.commons.lang3.StringUtils;

public enum KitStatus {
  UNUSED("Unused"),
  REPURPOSED("Repurposed"),
  COLLECTED_BUT_UNRECEIVED("Evidence collected (not received by law enforcement)"),
  REQUIRES_LE_DATA("Requires law enforcement data"),
  REQUIRES_RELEASE_FOR_LEGAL_REVIEW("Requires release for prosecutorial review"),
  AWAITING_LEGAL_REVIEW("Awaiting prosecutorial review"),
  WILL_NOT_BE_ANALYZED("Does not meet submission requirements"),
  READY_TO_SEND_FOR_ANALYSIS("Ready to send to lab"),
  SENT_TO_BE_ANALYZED("Sent to lab"),
  BEING_ANALYZED("Being analyzed by lab"),
  ANALYZED("Analyzed by lab"),
  DESTROYED("Destroyed");

  private final String label;

  private KitStatus(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }

  public static KitStatus getStatus(SexualAssaultKit kit) {
    if (hasEventType(kit, EventType.REPURPOSE)) {
      return REPURPOSED;
    }

    if (hasEventType(kit, EventType.DESTROY)) {
      return DESTROYED;
    }

    if (MilestoneUtils.getMedicalCollectedDate(kit).isPresent()) {
      if (MilestoneUtils.getLeReceiveDate(kit).isPresent()) {
        if (isLeDataComplete(kit.getLeDetails())) {
          if (isFlaggedByLeAsNotMeetingSubmissionCriteria(kit)) {
            if (!MilestoneUtils.getReleasedForProsecutorReviewDate(kit).isPresent()) {
              return REQUIRES_RELEASE_FOR_LEGAL_REVIEW;
            }
            if (!MilestoneUtils.getProsecutorHasAgreedDate(kit).isPresent()) {
              return AWAITING_LEGAL_REVIEW;
            }
            if (Boolean.TRUE.equals(kit.getLegalDetails().getProsecutorAgrees())) {
              return WILL_NOT_BE_ANALYZED;
            }
          }
          if (MilestoneUtils.getLabCompleteDate(kit).isPresent()) {
            return ANALYZED;
          }
          if (MilestoneUtils.getLabReceiveDate(kit).isPresent()) {
            return BEING_ANALYZED;
          }
          if (MilestoneUtils.getLeSubmitDate(kit).isPresent()) {
            return SENT_TO_BE_ANALYZED;
          }
          return READY_TO_SEND_FOR_ANALYSIS;
        }
        return REQUIRES_LE_DATA;
      }
      return COLLECTED_BUT_UNRECEIVED;
    }
    return UNUSED;
  }

  private static boolean isFlaggedByLeAsNotMeetingSubmissionCriteria(SexualAssaultKit kit) {
    return kit.getLeDetails() != null && Boolean.FALSE.equals(kit.getLeDetails().getMeetsSubmissionCriteria());
  }

  private static boolean isLeDataComplete(LawEnforcementDetails leDetails) {
    return StringUtils.isNotBlank(leDetails.getCaseNumber())
      && StringUtils.isNotBlank(leDetails.getInvestigator())
      && StringUtils.isNotBlank(leDetails.getCrime())
      && leDetails.getMeetsSubmissionCriteria() != null
      && (leDetails.getCrimeDate() != null && !leDetails.getCrimeDate().isAfter(LocalDate.now()));
  }

  private static boolean hasEventType(SexualAssaultKit kit, EventType eventType) {
    return kit.getChainOfCustody().stream().anyMatch(c -> eventType == c.getEventType());
  }
}
