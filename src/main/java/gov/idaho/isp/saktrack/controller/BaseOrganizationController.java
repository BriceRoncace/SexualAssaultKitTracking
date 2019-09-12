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

package gov.idaho.isp.saktrack.controller;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.util.UserUtils;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;

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
