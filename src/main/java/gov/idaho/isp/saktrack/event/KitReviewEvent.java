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

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.user.organization.LegalUser;
import java.time.LocalDateTime;

public class KitReviewEvent {
  private final LegalUser user;
  private final SexualAssaultKit kit;
  private final LocalDateTime timestamp;

  public KitReviewEvent(LegalUser user, SexualAssaultKit kit) {
    this.user = user;
    this.kit = kit;
    this.timestamp = LocalDateTime.now();
  }

  public LegalUser getUser() {
    return user;
  }

  public SexualAssaultKit getSexualAssaultKit() {
    return kit;
  }

  public LocalDateTime getTimestamp() {
    return timestamp;
  }
}
