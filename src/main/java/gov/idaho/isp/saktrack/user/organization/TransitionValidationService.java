package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.dto.CreateKitEventDetails;
import gov.idaho.isp.saktrack.dto.EventDetails;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.organization.OrganizationType;

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