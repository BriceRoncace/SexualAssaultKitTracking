package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.organization.Organization;
import gov.idaho.isp.saktrack.user.User;
import gov.idaho.isp.saktrack.user.UserUtils;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;

public abstract class BaseOrganizationController extends BaseController {
  public final OrganizationUserRepository organizationUserRepository;

  public BaseOrganizationController(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  public boolean canAndShouldSeeUnverifiedUserMessage(Organization org, User user) {
    if (org == null || user == null) {
      return false;
    }
    return UserUtils.isAdminOrOrgAdmin(user) && hasUnverifiedUsers(org);
  }

  private boolean hasUnverifiedUsers(Organization organization) {
    return organizationUserRepository.findUnverifiedUserByOrganization(organization.getId()).size() > 0;
  }
}
