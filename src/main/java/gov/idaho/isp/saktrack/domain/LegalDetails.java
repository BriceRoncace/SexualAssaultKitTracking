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

import gov.idaho.isp.saktrack.domain.LawEnforcementDetails.NonSubmissionReason;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import java.io.Serializable;
import java.time.LocalDate;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
public class LegalDetails implements SexualAssaultKitAware, Serializable {
  @Id
  @GeneratedValue
  private Long id;

  @JoinColumn(name = "releasedForProsecutorReview")
  private LocalDate releasedForReview;

  @Enumerated(EnumType.STRING)
  private NonSubmissionReason nonSubmissionReason;

  @ManyToOne(cascade = CascadeType.ALL)
  @JoinColumn(name = "reviewingProsecutorOrgId")
  private Organization reviewingOrganization;

  private String reviewingProsecutor;

  private Boolean prosecutorAgrees;

  private String prosecutorNotes;

  @LocalDatePast(message = "{reviewFinalized.in.future}")
  private LocalDate reviewFinalized;

  @OneToOne(mappedBy = "legalDetails")
  private SexualAssaultKit sexualAssaultKit;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public LocalDate getReleasedForReview() {
    return releasedForReview;
  }

  public void setReleasedForReview(LocalDate releasedForReview) {
    this.releasedForReview = releasedForReview;
  }

  public NonSubmissionReason getNonSubmissionReason() {
    return nonSubmissionReason;
  }

  public void setNonSubmissionReason(NonSubmissionReason nonSubmissionReason) {
    this.nonSubmissionReason = nonSubmissionReason;
  }

  public Organization getReviewingOrganization() {
    return reviewingOrganization;
  }

  public void setReviewingOrganization(Organization reviewingOrganization) {
    this.reviewingOrganization = reviewingOrganization;
  }

  public Boolean getProsecutorAgrees() {
    return prosecutorAgrees;
  }

  public void setProsecutorAgrees(Boolean prosecutorAgrees) {
    this.prosecutorAgrees = prosecutorAgrees;
  }

  public String getProsecutorNotes() {
    return prosecutorNotes;
  }

  public void setProsecutorNotes(String prosecutorNotes) {
    this.prosecutorNotes = prosecutorNotes;
  }

  public String getReviewingProsecutor() {
    return reviewingProsecutor;
  }

  public void setReviewingProsecutor(String reviewingProsecutor) {
    this.reviewingProsecutor = reviewingProsecutor;
  }

  public LocalDate getReviewFinalized() {
    return reviewFinalized;
  }

  public void setReviewFinalized(LocalDate reviewFinalized) {
    this.reviewFinalized = reviewFinalized;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }
}