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

package gov.idaho.isp.saktrack.domain.jurisdiction;

import java.io.Serializable;
import java.util.Objects;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.validation.constraints.NotBlank;

@Entity
@Table(indexes = {@Index(name = "AK_Jurisdiction_name_type", columnList="name,type", unique = true)})
public class Jurisdiction implements Serializable {
  public enum Type {
    COUNTY("County"),
    RESERVATION("Reservation"),
    STATEWIDE("Statewide"),
    FEDERAL("Federal");

    private final String label;

    private Type(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  @Id
  @GeneratedValue
  private Long id;

  @NotBlank(message = "{name.blank}")
  private String name;

  @Enumerated(EnumType.STRING)
  private Type type;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public String getDisplayName() {
    return name + " " + (type != null ? type.getLabel() : "");
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Type getType() {
    return type;
  }

  public void setType(Type type) {
    this.type = type;
  }

  public boolean isStatewide() {
    return Type.STATEWIDE == type;
  }

  @Override
  public int hashCode() {
    int hash = 3;
    hash = 37 * hash + Objects.hashCode(this.name);
    hash = 37 * hash + Objects.hashCode(this.type);
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
    final Jurisdiction other = (Jurisdiction) obj;
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
    return "Jurisdiction{" + "id=" + id + ", name=" + name + ", type=" + type + '}';
  }
}
