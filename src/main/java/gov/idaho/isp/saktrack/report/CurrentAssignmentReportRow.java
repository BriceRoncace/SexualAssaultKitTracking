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
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import java.time.LocalDate;
import java.util.Optional;

public class CurrentAssignmentReportRow {
  private final SexualAssaultKit kit;

  public CurrentAssignmentReportRow(SexualAssaultKit kit) {
    this.kit = kit;
  }

  public String getName() {
    if (kit.getCurrentAssignment() != null) {
      return kit.getCurrentAssignment().getName();
    }
    return null;
  }

  public OrganizationType getType() {
    if (kit.getCurrentAssignment() != null) {
      return kit.getCurrentAssignment().getType();
    }
    return null;
  }

  public Long getId() {
    return kit.getId();
  }

  public String getSerialNumber() {
    return kit.getSerialNumber();
  }

  public LocalDate getExpirationDate() {
    return kit.getExpirationDate();
  }

  public LocalDate getCreatedDate() {
    return findEventDate(kit, ChainOfCustodyEvent.EventType.CREATE);
  }

  private LocalDate findEventDate(SexualAssaultKit kit, ChainOfCustodyEvent.EventType eventType) {
    Optional<ChainOfCustodyEvent> event = kit.getChainOfCustody().stream().filter(e -> eventType.equals(e.getEventType())).findFirst();
    return event.isPresent() ? event.get().getEventDate() : null;
  }
}