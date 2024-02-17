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

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.util.EventUtil;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import gov.idaho.isp.saktrack.validation.ReviewingProsecutorOrganizationSetIfRequired;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.NamedAttributeNode;
import jakarta.persistence.NamedEntityGraph;
import jakarta.persistence.NamedQueries;
import jakarta.persistence.NamedQuery;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.OrderColumn;
import jakarta.persistence.Table;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
@Table(indexes = {@Index(name = "AK_SexualAssaultKit_serialNumber", columnList="serialNumber", unique = true)})
@NamedQueries({
  @NamedQuery(name = "SexualAssaultKit.findByCurrentAssignmentId", query = "from SexualAssaultKit where currentAssignment.id = ?1 order by serialNumber")
})
@NamedEntityGraph(name = "sak.with.events", attributeNodes = @NamedAttributeNode("chainOfCustody"))
@ReviewingProsecutorOrganizationSetIfRequired
public class SexualAssaultKit implements SexualAssaultKitAware, Serializable {

  @Id
  @GeneratedValue
  private Long id;

  @NotNull(message = "{serialNumber.null}")
  @Pattern(regexp = "\\d+", message = "{serialNumber.not.number}")
  private String serialNumber;

  @NotNull(message = "{expirationDate.null}")
  private LocalDate expirationDate;

  private LocalDateTime lastModified;

  private String lastModifiedBy;

  @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER, mappedBy = "sexualAssaultKit")
  @Valid
  @OrderColumn(name = "sortOrder")
  private final List<ChainOfCustodyEvent> chainOfCustody = new ArrayList<>();

  private boolean questionableEvents;

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "medicalDetailsId")
  @Valid
  private MedicalDetails medicalDetails = new MedicalDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "leDetailsId")
  @Valid
  private LawEnforcementDetails leDetails = new LawEnforcementDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "legalDetailsId")
  @Valid
  private LegalDetails legalDetails = new LegalDetails();

  @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "labDetailsId")
  @Valid
  private LabDetails labDetails = new LabDetails();

  @ManyToOne
  @JoinColumn(name = "organizationId")
  private Organization currentAssignment;

  public SexualAssaultKit() {
  }

  public SexualAssaultKit(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public ChainOfCustodyEvent getCurrentCustody() {
    return !chainOfCustody.isEmpty() ? chainOfCustody.get(chainOfCustody.size()-1) : null;
  }

  public void addChainOfCustodyEvent(ChainOfCustodyEvent event) {
    event.setSexualAssaultKit(this);
    chainOfCustody.add(event);
  }

  public KitStatus getStatus() {
    return KitStatus.getStatus(this);
  }

  public Jurisdiction getJurisdiction() {
    return currentAssignment.getJurisdiction();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getSerialNumber() {
    return serialNumber;
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  public LocalDate getExpirationDate() {
    return expirationDate;
  }

  public void setExpirationDate(LocalDate expirationDate) {
    this.expirationDate = expirationDate;
  }

  public LocalDateTime getLastModified() {
    return lastModified;
  }

  public void setLastModified(LocalDateTime lastModified) {
    this.lastModified = lastModified;
  }

  public String getLastModifiedBy() {
    return lastModifiedBy;
  }

  public void setLastModifiedBy(String lastModifiedBy) {
    this.lastModifiedBy = lastModifiedBy;
  }

  public List<ChainOfCustodyEvent> getChainOfCustody() {
    return chainOfCustody;
  }

  public boolean getQuestionableEvents() {
    return questionableEvents;
  }

  public void setQuestionableEvents(boolean questionableEvents) {
    this.questionableEvents = questionableEvents;
  }

  public boolean hasCalculatedMissingEvents() {
    return EventUtil.hasMissingSendEvents(chainOfCustody);
  }

  public MedicalDetails getMedicalDetails() {
    if (medicalDetails == null) {
      medicalDetails = new MedicalDetails();
    }
    return medicalDetails;
  }

  public void setMedicalDetails(MedicalDetails medicalDetails) {
    this.medicalDetails = medicalDetails;
  }

  public LawEnforcementDetails getLeDetails() {
    if (leDetails == null) {
      leDetails = new LawEnforcementDetails();
    }
    return leDetails;
  }

  public void setLeDetails(LawEnforcementDetails leDetails) {
    this.leDetails = leDetails;
  }

  public LegalDetails getLegalDetails() {
    if (legalDetails == null) {
      legalDetails = new LegalDetails();
    }
    return legalDetails;
  }

  public void setLegalDetails(LegalDetails legalDetails) {
    this.legalDetails = legalDetails;
  }

  public LabDetails getLabDetails() {
    if (labDetails == null) {
      labDetails = new LabDetails();
    }
    return labDetails;
  }

  public void setLabDetails(LabDetails labDetails) {
    this.labDetails = labDetails;
  }

  public Organization getCurrentAssignment() {
    return currentAssignment;
  }

  public void setCurrentAssignment(Organization currentAssignment) {
    this.currentAssignment = currentAssignment;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return this;
  }

  public boolean isDestroyable() {
    KitStatus status = getStatus();
    return leDetails != null &&
      leDetails.getPlannedDestructionDate() != null &&
      !leDetails.getPlannedDestructionDate().isAfter(LocalDate.now()) &&
      (status == KitStatus.ANALYZED || status == KitStatus.WILL_NOT_BE_ANALYZED);
  }

  public Long getTimeAtLe() {
    return MilestoneUtils.getTimeAtLawEnforcement(this).orElse(null);
  }

  @Override
  public String toString() {
    return "SexualAssaultKit{" + "id=" + id + ", serialNumber=" + serialNumber + ", chainOfCustody=" + chainOfCustody + ", medicalDetails=" + medicalDetails + ", leDetails=" + leDetails + ", labDetails=" + labDetails + ", currentAssignment=" + currentAssignment + '}';
  }
}
