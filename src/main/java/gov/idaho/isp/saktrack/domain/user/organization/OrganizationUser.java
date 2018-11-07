package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;

public interface OrganizationUser extends User {
  Organization getOrganization();
  boolean isOrganizationContact();

  /*
   * An organization user is active if their user account is enabled and their
   * associated organization is also enabled.
   */
  boolean isActive();

  boolean isVerified();
  String getPasskey();
}
