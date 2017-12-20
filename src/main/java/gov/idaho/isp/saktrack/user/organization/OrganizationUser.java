package gov.idaho.isp.saktrack.user.organization;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.user.User;

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
