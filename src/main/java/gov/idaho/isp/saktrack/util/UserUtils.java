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

import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.exception.UnexpectedUserException;
import java.util.Objects;

public class UserUtils {
  public static void verifyUserTypeOrThrowException(User user, User.Type userType) {
    if (user == null || user.getType() != userType) {
      throw new UnexpectedUserException("Expected user of type: [" + userType + "].  User given: [" + user + "]", "This action requires a " + userType.getLabel() + " user.");
    }
  }

  public static boolean isSameUserEntity(User user1, User user2) {
    return Objects.equals(user1.getId(), user2.getId());
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