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

package gov.idaho.isp.saktrack.util;

import gov.idaho.isp.saktrack.domain.organization.Organization;

public class MissingSendEvent {
  private Organization from;
  private Organization to;

  public MissingSendEvent(Organization from, Organization to) {
    this.from = from;
    this.to = to;
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
}
