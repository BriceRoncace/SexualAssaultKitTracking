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

package gov.idaho.isp.saktrack.domain.organization;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.validation.PasswordPolicy;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.NamedQueries;
import jakarta.persistence.NamedQuery;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(indexes = {@Index(name = "AK_Organization_name", columnList="name", unique = true)})
@NamedQueries({
  @NamedQuery(name = "Organization.findLegalByJusidictionId", query = "from Organization where type = 'LEGAL' and jurisdiction.id = ?1"),
  @NamedQuery(name = "Organization.findAssignableOrganizations", query = "from Organization where type != 'LEGAL' order by name")
})
public class Organization implements Serializable {
  @Id
  @GeneratedValue
  private Long id;

  @NotNull(message = "{type.null}")
  @Enumerated(EnumType.STRING)
  private OrganizationType type;

  @NotBlank(message = "{name.blank}")
  private String name;

  @PasswordPolicy
  @NotBlank(message = "{passkey.blank}")
  private String passkey;

  private boolean enabled;

  @NotNull(message = "{jurisdiction.null}")
  @ManyToOne
  @JoinColumn(name = "jurisdictionId")
  private Jurisdiction jurisdiction;

  public Organization () {}

  public Organization(Organization org) {
    this.id = org.getId();
    this.type = org.getType();
    this.name = org.getName();
    this.passkey = org.getPasskey();
    this.enabled = org.getEnabled();
    this.jurisdiction = org.getJurisdiction();
  }

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public OrganizationType getType() {
    return type;
  }

  public void setType(OrganizationType type) {
    this.type = type;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getPasskey() {
    return passkey;
  }

  public void setPasskey(String passkey) {
    this.passkey = passkey;
  }

  public boolean getEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public Jurisdiction getJurisdiction() {
    return jurisdiction;
  }

  public void setJurisdiction(Jurisdiction jurisdiction) {
    this.jurisdiction = jurisdiction;
  }

  public boolean isStatewide() {
    return jurisdiction != null ? jurisdiction.isStatewide() : false;
  }

  @Override
  public int hashCode() {
    int hash = 7;
    hash = 29 * hash + Objects.hashCode(this.type);
    hash = 29 * hash + Objects.hashCode(this.name);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Organization other = (Organization) obj;
    if (!Objects.equals(this.name, other.name)) {
      return false;
    }
    if (this.type != other.type) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return name;
  }
}