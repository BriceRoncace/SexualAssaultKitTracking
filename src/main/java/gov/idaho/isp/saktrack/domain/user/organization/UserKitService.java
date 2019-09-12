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

package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;

interface UserKitService {
  void create(LabUser user, CreateKitEventDetails eventDetails) throws SexualAssaultKitTrackingException;
  void delete(LabUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  void send(OrganizationUser user, EventDetails eventDetails, OrganizationType destinationType) throws SexualAssaultKitTrackingException;
  void receive(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  void destroy(LawEnforcementUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  void repurpose(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  void releaseForProsecutorReview(LawEnforcementUser user, SexualAssaultKit kit) throws SexualAssaultKitTrackingException;
  void review(LegalUser user, SexualAssaultKit kit, String notes, boolean agree) throws SexualAssaultKitTrackingException;
}