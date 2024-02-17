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
import gov.idaho.isp.saktrack.util.DateFormatter;
import gov.idaho.isp.saktrack.validation.ChainOfCustodyEventValid;
import gov.idaho.isp.saktrack.validation.LocalDatePast;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@EntityListeners(LastModifiedEntityListener.class)
@ChainOfCustodyEventValid
public class ChainOfCustodyEvent implements SexualAssaultKitAware, Serializable {

  public enum EventType {
    CREATE("Create", "created", "kit-new"),
    SEND("Send", "sent", "kit-incoming"),
    RECEIVE("Receive", "received", "kit-in-process"),
    DESTROY("Destroy", "destroyed", "kit-in-process"),
    REPURPOSE("Repurpose", "repurposed", "kit-in-process");

    private final String label;
    private final String pastTenseLabel;
    private final String viewName;

    private EventType(String label, String pastTenseLabel, String viewName) {
      this.label = label;
      this.pastTenseLabel = pastTenseLabel;
      this.viewName = viewName;
    }

    public String getLabel() {
      return label;
    }

    public String getPastTenseLabel() {
      return pastTenseLabel;
    }

    public String getViewName() {
      return viewName;
    }
  }

  @Id
  @GeneratedValue
  private Long id;

  @NotNull(message = "{eventType.null}")
  @Enumerated(EnumType.STRING)
  private EventType eventType;

  @NotBlank(message = "{event.actor.blank}")
  private String actor;

  @NotNull(message = "{event.actor.organization.null}")
  @ManyToOne
  @JoinColumn(name = "actorOrganizationId")
  private Organization actorOrganization;

  @ManyToOne
  @JoinColumn(name = "fromOrganizationId")
  private Organization from;

  @ManyToOne
  @JoinColumn(name = "toOrganizationId")
  private Organization to;

  @NotNull(message = "{eventDate.null}")
  @LocalDatePast
  private LocalDate eventDate;

  @NotNull(message = "{digitalTimestamp.null}")
  private LocalDateTime digitalTimestamp;

  private String notes;

  private boolean grabbedOutOfOrder;

  @Enumerated(EnumType.STRING)
  private EventFlag eventFlag;

  @ManyToOne
  @JoinColumn(name = "sexualAssaultKitId")
  private SexualAssaultKit sexualAssaultKit;

  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public EventType getEventType() {
    return eventType;
  }

  public void setEventType(EventType eventType) {
    this.eventType = eventType;
  }

  public String getActor() {
    return actor;
  }

  public void setActor(String actor) {
    this.actor = actor;
  }

  public Organization getActorOrganization() {
    return actorOrganization;
  }

  public void setActorOrganization(Organization actorOrganization) {
    this.actorOrganization = actorOrganization;
  }

  public Organization getFrom() {
    return from;
  }

  public void setFrom(Organization from) {
    this.from = from;
  }

  public Organization getTo() {
    return to;
  }

  public void setTo(Organization to) {
    this.to = to;
  }

  public LocalDate getEventDate() {
    return eventDate;
  }

  public void setEventDate(LocalDate eventDate) {
    this.eventDate = eventDate;
  }

  public LocalDateTime getDigitalTimestamp() {
    return digitalTimestamp;
  }

  public void setDigitalTimestamp(LocalDateTime digitalTimestamp) {
    this.digitalTimestamp = digitalTimestamp;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public boolean getGrabbedOutOfOrder() {
    return grabbedOutOfOrder;
  }

  public void setGrabbedOutOfOrder(boolean grabbedOutOfOrder) {
    this.grabbedOutOfOrder = grabbedOutOfOrder;
  }

  public EventFlag getEventFlag() {
    return eventFlag;
  }

  public void setEventFlag(EventFlag eventFlag) {
    this.eventFlag = eventFlag;
  }

  @Override
  public SexualAssaultKit getSexualAssaultKit() {
    return sexualAssaultKit;
  }

  public void setSexualAssaultKit(SexualAssaultKit sexualAssaultKit) {
    this.sexualAssaultKit = sexualAssaultKit;
  }

  public String getDescription(boolean showActor) {
    StringBuilder sb = new StringBuilder();
    if (showActor) {
      sb.append(actor);
    }

    if (actorOrganization != null) {
      if (showActor) {
        sb.append(" (with ");
      }

      sb.append(actorOrganization.getName());

      if (showActor) {
        sb.append(")");
      }
    }

    sb.append(" ").append(eventType.getPastTenseLabel()).append(" the kit");

    if (eventType == EventType.SEND && to != null) {
      sb.append(" to ").append(to.getName());
    }

    if (eventType == EventType.RECEIVE && from != null) {
      sb.append(" from ").append(from.getName());
    }

    sb.append(".");
    return sb.toString();
  }

  public String prettyPrint(boolean showActor) {
    StringBuilder sb = new StringBuilder();
    sb.append(DateFormatter.format(eventDate)).append(": ");
    sb.append(getDescription(showActor));
    if (grabbedOutOfOrder) {
      sb.append(" This event has been flagged as questionable.");
    }
    return sb.toString();
  }

  @Override
  public int hashCode() {
    int hash = 3;
    hash = 67 * hash + Objects.hashCode(this.eventType);
    hash = 67 * hash + Objects.hashCode(this.actor);
    hash = 67 * hash + Objects.hashCode(this.actorOrganization);
    hash = 67 * hash + Objects.hashCode(this.from);
    hash = 67 * hash + Objects.hashCode(this.to);
    hash = 67 * hash + Objects.hashCode(this.eventDate);
    hash = 67 * hash + Objects.hashCode(this.digitalTimestamp);
    hash = 67 * hash + Objects.hashCode(this.notes);
    hash = 67 * hash + (this.grabbedOutOfOrder ? 1 : 0);
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
    final ChainOfCustodyEvent other = (ChainOfCustodyEvent) obj;
    if (this.grabbedOutOfOrder != other.grabbedOutOfOrder) {
      return false;
    }
    if (!Objects.equals(this.actor, other.actor)) {
      return false;
    }
    if (!Objects.equals(this.notes, other.notes)) {
      return false;
    }
    if (this.eventType != other.eventType) {
      return false;
    }
    if (!Objects.equals(this.actorOrganization, other.actorOrganization)) {
      return false;
    }
    if (!Objects.equals(this.from, other.from)) {
      return false;
    }
    if (!Objects.equals(this.to, other.to)) {
      return false;
    }
    if (!Objects.equals(this.eventDate, other.eventDate)) {
      return false;
    }
    if (!Objects.equals(this.digitalTimestamp, other.digitalTimestamp)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "ChainOfCustodyEvent{" + "id=" + id + ", eventType=" + eventType + ", actor=" + actor + ", actorOrganization=" + actorOrganization + ", from=" + from + ", to=" + to + ", eventDate=" + eventDate + ", digitalTimestamp=" + digitalTimestamp + ", notes=" + notes + ", eventFlag=" + eventFlag + '}';
  }
}
