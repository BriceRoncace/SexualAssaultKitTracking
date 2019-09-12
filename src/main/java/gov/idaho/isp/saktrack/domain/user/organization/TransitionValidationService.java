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
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;

interface TransitionValidationService {
  KitValidationStrategy getCreateValidationStrategy(LabUser user, CreateKitEventDetails eventDetails) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getDeleteValidationStrategy(LabUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getSendValidationStrategy(OrganizationUser user, EventDetails eventDetails, Organization sendTo, OrganizationType destinationType) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getReceiveValidationStrategy(OrganizationUser user, EventDetails eventDetails, Organization sendFrom) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getDestroyValidationStrategy(LawEnforcementUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getRepurposeValidationStrategy(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getReleaseForReviewValidationStrategy(LawEnforcementUser user) throws SexualAssaultKitTrackingException;
  KitValidationStrategy getReviewValidationStrategy(LegalUser user, String notes) throws SexualAssaultKitTrackingException;
}

@FunctionalInterface
interface KitValidationStrategy {
  void validate(SexualAssaultKit kit, String serialNumber) throws SexualAssaultKitTrackingException;
}