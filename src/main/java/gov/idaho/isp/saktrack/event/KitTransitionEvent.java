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

package gov.idaho.isp.saktrack.event;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import java.time.LocalDateTime;

public abstract class KitTransitionEvent {
  private final OrganizationUser user;
  private final EventDetails eventDetails;
  private final LocalDateTime timestamp;

  public KitTransitionEvent(OrganizationUser user, EventDetails eventDetails) {
    this.user = user;
    this.eventDetails = eventDetails;
    this.timestamp = LocalDateTime.now();
  }

  public OrganizationUser getUser() {
    return user;
  }

  public EventDetails getEventDetails() {
    return eventDetails;
  }

  public LocalDateTime getTransitionEventTimestamp() {
    return timestamp;
  }
}
