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

import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.NonSubmissionReasonSetIfRequired;
import gov.idaho.isp.saktrack.validation.ProsecutorReviewComplete;
import gov.idaho.isp.saktrack.validation.group.ReleaseForReview;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.io.Serializable;
import java.time.LocalDate;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
@NonSubmissionReasonSetIfRequired
@ProsecutorReviewComplete(groups = {SendKit.class})
public class LawEnforcementDetails implements SexualAssaultKitAware, Serializable {

  public enum NonSubmissionReason implements HasLabel {
    NO_EVIDENCE("There is no evidence to support a crime being committed."),
    NOT_A_CRIME("It is no longer being investigated as a crime."),
    NO_TESTING("An adult victim expressly indicates that no further forensic examination or testing occur.");

    private final String label;

    private NonSubmissionReason(String label) {
      this.label = label;
    }

    @Override
    public String getLabel() {
      return label;
    }
  }

  @Id
  @GeneratedValue
  private Long id;

  @NotNull(message = "{caseNumber.null}", groups = {SendKit.class, ReleaseForReview.class})
  private String caseNumber;

  @NotNull(message = "{investigator.null}", groups = {SendKit.class, ReleaseForReview.class})
  private String investigator;

  @NotNull(message = "{crime.null}", groups = {SendKit.class, ReleaseForReview.class})
  private String crime;

  @NotNull(message = "{crimeDate.null}", groups = {SendKit.class, ReleaseForReview.class})
  @LocalDatePast(message = "{crimeDate.in.future}")
  private LocalDate crimeDate;

  private String outsourcedLabName;

  private LocalDate plannedDestructionDate;

  @Column(name = "destructNotificationReq")
  private Boolean plannedDestructionNotificationRequested;

  @NotNull(message = "{meetsSubmissionCriteria.null}", groups = {SendKit.class, ReleaseForReview.class})
  private Boolean meetsSubmissionCriteria;

  @NotNull(message = "{nonSubmissionReason.null}", groups = {ReleaseForReview.class})
  @Enumerated(EnumType.STRING)
  private NonSubmissionReason nonSubmissionReason;

  @OneToOne(mappedBy = "leDetails")
  private SexualAssaultKit sexualAssaultKit;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getCaseNumber() {
    return caseNumber;
  }

  public void setCaseNumber(String caseNumber) {
    this.caseNumber = caseNumber;
  }

  public String getInvestigator() {
    return investigator;
  }

  public void setInvestigator(String investigator) {
    this.investigator = investigator;
  }

  public String getCrime() {
    return crime;
  }

  public void setCrime(String crime) {
    this.crime = crime;
  }

  public LocalDate getCrimeDate() {
    return crimeDate;
  }

  public void setCrimeDate(LocalDate crimeDate) {
    this.crimeDate = crimeDate;
  }

  public String getOutsourcedLabName() {
    return outsourcedLabName;
  }

  public void setOutsourcedLabName(String outsourcedLabName) {
    this.outsourcedLabName = outsourcedLabName;
  }

  public LocalDate getPlannedDestructionDate() {
    return plannedDestructionDate;
  }

  public void setPlannedDestructionDate(LocalDate plannedDestructionDate) {
    this.plannedDestructionDate = plannedDestructionDate;
  }

  public Boolean getPlannedDestructionNotificationRequested() {
    return plannedDestructionNotificationRequested;
  }

  public void setPlannedDestructionNotificationRequested(Boolean plannedDestructionNotificationRequested) {
    this.plannedDestructionNotificationRequested = plannedDestructionNotificationRequested;
  }

  public Boolean getMeetsSubmissionCriteria() {
    return meetsSubmissionCriteria;
  }

  public void setMeetsSubmissionCriteria(Boolean meetsSubmissionCriteria) {
    this.meetsSubmissionCriteria = meetsSubmissionCriteria;
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return nonSubmissionReason;
  }

  public void setNonSubmissionReason(NonSubmissionReason nonSubmissionReason) {
    this.nonSubmissionReason = nonSubmissionReason;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }
}