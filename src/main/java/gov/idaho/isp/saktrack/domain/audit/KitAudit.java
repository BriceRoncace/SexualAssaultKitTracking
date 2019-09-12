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

package gov.idaho.isp.saktrack.domain.audit;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;

@Entity
public class KitAudit implements Serializable {
  @Id
  @GeneratedValue
  private Long id;

  private Long kitId;

  private String displayName;

  private LocalDateTime modified;

  @ElementCollection(fetch = FetchType.EAGER)
  @CollectionTable(name = "AuditChanges",  joinColumns = @JoinColumn(name = "kitAuditId"))
  @Column(length = 2000)
  private List<String> changes = new ArrayList<>();

  @Column(length = 1000)
  private String notes;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Long getKitId() {
    return kitId;
  }

  public void setKitId(Long kitId) {
    this.kitId = kitId;
  }

  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  public LocalDateTime getModified() {
    return modified;
  }

  public void setModified(LocalDateTime modified) {
    this.modified = modified;
  }

  public List<String> getChanges() {
    return changes;
  }

  public void setChanges(List<String> changes) {
    this.changes = changes;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  @Override
  public String toString() {
    return "KitAudit{" + "id=" + id + ", kitId=" + kitId + ", displayName=" + displayName + ", modified=" + modified + ", changes=" + changes + ", notes=" + notes + '}';
  }
}
