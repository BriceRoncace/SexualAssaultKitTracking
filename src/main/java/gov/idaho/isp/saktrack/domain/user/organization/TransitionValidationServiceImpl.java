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

import gov.idaho.isp.saktrack.domain.KitStatus;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.domain.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.service.ValidationService;
import gov.idaho.isp.saktrack.util.EventUtil;
import gov.idaho.isp.saktrack.util.MilestoneUtils;
import gov.idaho.isp.saktrack.validation.group.ReleaseForReview;
import gov.idaho.isp.saktrack.validation.group.SendKit;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Service
class TransitionValidationServiceImpl implements TransitionValidationService {
  private final ValidationService validationService;

  private final List<KitStatus> needsLabValidationStatuses = Arrays.asList(KitStatus.ANALYZED, KitStatus.BEING_ANALYZED);

  public TransitionValidationServiceImpl(ValidationService validationService) {
    this.validationService = validationService;
  }

  @Override
  public KitValidationStrategy getCreateValidationStrategy(LabUser user, CreateKitEventDetails eventDetails) throws SexualAssaultKitTrackingException {
    if (user.getOrganization() == null) {
      throw new SexualAssaultKitTrackingException("Kit cannot be created by a lab user who is not currently assigned to an organization.");
    }

    return (SexualAssaultKit kit, String serialNumber) -> {
      basicValidation(kit, serialNumber, user);
    };
  }

  @Override
  public KitValidationStrategy getDeleteValidationStrategy(LabUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    if (user.getOrganization() == null) {
      throw new SexualAssaultKitTrackingException("Kit cannot be deleted by a lab user who is not currently assigned to an organization.");
    }

    return (SexualAssaultKit kit, String serialNumber) -> {
      basicValidation(kit, serialNumber, user);
      deleteKitValidation(kit, serialNumber);
    };
  }

  @Override
  public KitValidationStrategy getSendValidationStrategy(OrganizationUser user, EventDetails eventDetails, Organization sendTo, OrganizationType destinationType) throws SexualAssaultKitTrackingException {
    if (!destinationType.equals(sendTo.getType())) {
      throw new SexualAssaultKitTrackingException("Destination organization is not a " + destinationType.getLabel() + " organization.");
    }

    return (SexualAssaultKit kit, String serialNumber) -> {
      basicValidation(kit, serialNumber, user);
      sendKitValidation(kit, serialNumber, user, eventDetails, sendTo);
    };
  }

  @Override
  public KitValidationStrategy getReceiveValidationStrategy(OrganizationUser user, EventDetails eventDetails, Organization sendFrom) throws SexualAssaultKitTrackingException {
    return (SexualAssaultKit kit, String serialNumber) -> {
      if (kit == null) {
        throw new SexualAssaultKitTrackingException("Serial number " + serialNumber + " not found.");
      }
      if (!EventUtil.validReceiveDate(kit, user.getOrganization(), sendFrom, eventDetails)) {
        throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " has an invalid receive date.");
      }
    };
  }

  @Override
  public KitValidationStrategy getDestroyValidationStrategy(LawEnforcementUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    return (SexualAssaultKit kit, String serialNumber) -> {
      basicValidation(kit, serialNumber, user);
    };
  }

  @Override
  public KitValidationStrategy getRepurposeValidationStrategy(OrganizationUser user, EventDetails eventDetails) throws SexualAssaultKitTrackingException {
    return (SexualAssaultKit kit, String serialNumber) -> {
      basicValidation(kit, serialNumber, user);
    };
  }

  @Override
  public KitValidationStrategy getReleaseForReviewValidationStrategy(LawEnforcementUser user) throws SexualAssaultKitTrackingException {
    return (SexualAssaultKit kit, String serialNumber) -> {
      if (kit == null) {
        throw new SexualAssaultKitTrackingException("Serial number " + serialNumber + " not found.");
      }
      Set<String> errors = new HashSet<>();
      errors.addAll(validationService.validateLawEnforcementDetails(kit.getLeDetails(), ReleaseForReview.class));
      if (!errors.isEmpty()) {
        throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " is not valid.", errors);
      }
    };
  }

  @Override
  public KitValidationStrategy getReviewValidationStrategy(LegalUser user, String notes) throws SexualAssaultKitTrackingException {
    if (StringUtils.isBlank(notes)) {
      throw new SexualAssaultKitTrackingException("Notes must be provided when completing a legal review.");
    }

    return (SexualAssaultKit kit, String serialNumber) -> {
      if (kit == null) {
        throw new SexualAssaultKitTrackingException("Serial number " + serialNumber + " not found.");
      }
      if (Boolean.TRUE.equals(kit.getLeDetails().getMeetsSubmissionCriteria()) || kit.getLegalDetails().getReviewFinalized() != null) {
        throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " is not reviewable.");
      }
    };
  }

  private void basicValidation(SexualAssaultKit kit, String serialNumber, OrganizationUser user) {
    if (kit == null) {
      throw new SexualAssaultKitTrackingException("Serial number " + serialNumber + " not found.");
    }
    if (!user.getOrganization().equals(kit.getCurrentAssignment())) {
      throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " not currently assigned to the initiating organization.");
    }
  }

  private void deleteKitValidation(SexualAssaultKit kit, String serialNumber) {
    if (!KitStatus.UNUSED.equals(kit.getStatus())) {
      throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " cannot be deleted. Only unused kits can be deleted.");
    }
  }

  private void sendKitValidation(SexualAssaultKit kit, String serialNumber, OrganizationUser user, EventDetails eventDetails, Organization sendTo) {
    Set<String> errors = new HashSet<>();

    if (!EventUtil.validSendDate(kit, user.getOrganization(), sendTo, eventDetails)) {
      errors.add("Kit " + serialNumber + " has an invalid sent date.");
    }

    if (MilestoneUtils.getMedicalCollectedDate(kit).isPresent() && OrganizationType.MEDICAL.equals(sendTo.getType())) {
      errors.add("Kit " + serialNumber + " has been collected and cannot be sent back to a medical organization.");
    }

    if (KitStatus.COLLECTED_BUT_UNRECEIVED.equals(kit.getStatus())) {
      errors.addAll(validationService.validateMedicalDetails(kit.getMedicalDetails(), SendKit.class));
    }

    if (user.getOrganization().equals(kit.getMedicalDetails().getRequestingLeAgency())) {
      errors.addAll(validationService.validateLawEnforcementDetails(kit.getLeDetails(), SendKit.class));

      if (!KitStatus.READY_TO_SEND_FOR_ANALYSIS.equals(kit.getStatus())) {
        errors.add("Kit " + serialNumber + " is not ready to be sent to the Lab.");
      }
    }

    if (needsLabValidationStatuses.contains(kit.getStatus())) {
      errors.addAll(validationService.validateLabDetails(kit.getLabDetails(), SendKit.class));
    }

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("Kit " + serialNumber + " is not valid.", errors);
    }
  }
}
