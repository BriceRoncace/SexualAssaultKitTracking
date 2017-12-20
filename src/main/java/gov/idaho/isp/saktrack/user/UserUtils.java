package gov.idaho.isp.saktrack.user;

import gov.idaho.isp.saktrack.exception.UnexpectedUserException;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;

public class UserUtils {
  public static void verifyUserTypeOrThrowException(User user, User.Type userType) {
    if (user == null || user.getType() != userType) {
      throw new UnexpectedUserException("Expected user of type: [" + userType + "].  User given: [" + user + "]", "This action requires a " + userType.getLabel() + " user.");
    }
  }

  public static boolean isSameUserEntity(User user1, User user2) {
    return user1.getId().equals(user2.getId());
  }

  public static boolean isAdminOrOrgAdmin(User user) {
    return user.isAdmin() || user.isOrganizationAdmin();
  }

  public static boolean isLabUser(OrganizationUser orgUser) {
    return orgUser.getType() == OrganizationUser.Type.LAB;
  }

  public static boolean isLawEnforcementUser(OrganizationUser orgUser) {
    return orgUser.getType() == OrganizationUser.Type.LAW_ENFORCEMENT;
  }

  public static boolean isMedicalUser(OrganizationUser orgUser) {
    return orgUser.getType() == OrganizationUser.Type.MEDICAL;
  }

  public static boolean isContact(OrganizationUser user) {
    return user.isEnabled() && user.isOrganizationContact();
  }
}