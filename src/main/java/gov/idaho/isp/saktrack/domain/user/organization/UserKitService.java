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