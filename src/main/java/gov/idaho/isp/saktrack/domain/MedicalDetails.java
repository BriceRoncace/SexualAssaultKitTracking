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

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.io.Serializable;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.validation.constraints.NotNull;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
public class MedicalDetails implements SexualAssaultKitAware, Serializable {
  public enum VictimType implements HasLabel {
    NAMED("Named"),
    ANONYMOUS("Anonymous");

    private final String label;

    private VictimType(String label) {
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

  @LocalDatePast(message = "{collectionDate.in.future}")
  @NotNull(message = "{collectionDate.null}", groups = {SendKit.class})
  private LocalDate collectionDate;

  @Enumerated(EnumType.STRING)
  @NotNull(message = "{victimType.null}", groups = {SendKit.class})
  private VictimType victimType;

  @ManyToOne
  @JoinColumn(name = "requestingOrganizationId")
  @NotNull(message = "{org.id.null}", groups = {SendKit.class})
  private Organization requestingLeAgency;

  @OneToOne(mappedBy = "medicalDetails")
  private SexualAssaultKit sexualAssaultKit;

  public boolean isComplete() {
    return (victimType != null && requestingLeAgency != null && collectionDate != null);
  }

  public boolean isEmpty() {
    return (victimType == null && requestingLeAgency == null && collectionDate == null);
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public LocalDate getCollectionDate() {
    return collectionDate;
  }

  public void setCollectionDate(LocalDate collectionDate) {
    this.collectionDate = collectionDate;
  }

  public VictimType getVictimType() {
    return victimType;
  }

  public void setVictimType(VictimType victimType) {
    this.victimType = victimType;
  }

  public Organization getRequestingLeAgency() {
    return requestingLeAgency;
  }

  public void setRequestingLeAgency(Organization requestingLeAgency) {
    this.requestingLeAgency = requestingLeAgency;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }

  @Override
  public String toString() {
    return "MedicalDetails{" + "id=" + id + ", victimType=" + victimType + ", requestingLeAgency=" + requestingLeAgency + '}';
  }
}