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

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.EventFlag;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public class RequestingAgencyReportRow {
  private final SexualAssaultKit kit;

  public RequestingAgencyReportRow(SexualAssaultKit kit) {
    this.kit = kit;
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

  public Long getTimeAtMedical() {
    return MilestoneUtils.getTimeAtMedical(kit).orElse(null);
  }

  public Long getTimeInTransit() {
    return MilestoneUtils.getTimeFromMedicalToLe(kit).orElse(null);
  }

  public Long getTimeAtLawEnforcement() {
    return MilestoneUtils.getTimeAtLawEnforcement(kit).orElse(null);
  }

  public Long getTimeAtLab() {
    return MilestoneUtils.getTimeAtLab(kit).orElse(null);
  }

  public String getLastModifiedBy() {
    return kit.getLastModifiedBy();
  }

  public LocalDateTime getLastModified() {
    return kit.getLastModified();
  }

  public boolean isAtMedical() {
    return MilestoneUtils.getMedicalCollectedDate(kit).isPresent() && !MilestoneUtils.getMedicalSubmitDate(kit).isPresent();
  }

  public boolean isInTransit() {
    return MilestoneUtils.getMedicalSubmitDate(kit).isPresent() && !MilestoneUtils.getLeReceiveDate(kit).isPresent();
  }

  public boolean isAtLawEnforcement() {
    return MilestoneUtils.getLeReceiveDate(kit).isPresent() && !MilestoneUtils.getProsecutorAgreedDateOrSentToLabDate(kit).isPresent();
  }

  public boolean isAtLab() {
    return MilestoneUtils.getLabReceiveDate(kit).isPresent() && !MilestoneUtils.getLabCompleteDate(kit).isPresent();
  }

  public List<LocalDate> getLabReceivedDates() {
    return MilestoneUtils.getAllDatesForEventFlag(kit, EventFlag.LAB_RECEIVED);
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return kit.getLeDetails().getNonSubmissionReason();
  }

  public String getProsecutorNotes() {
    if (kit.getLeDetails() != null && kit.getLegalDetails().getProsecutorNotes() != null) {
      return kit.getLegalDetails().getProsecutorNotes();
    }
    return "";
  }

  public List<ChainOfCustodyEvent> getChainOfCustody() {
    return kit.getChainOfCustody();
  }

  public LocalDate getDnaDatabaseEntryDate() {
    return kit.getLabDetails().getDnaDatabaseEntryDate();
  }

  public LocalDate getDnaDatabaseHitDate() {
    return kit.getLabDetails().getDnaDatabaseHitDate();
  }

  public LocalDate getCollectionDate() {
    return kit.getMedicalDetails().getCollectionDate();
  }

}
