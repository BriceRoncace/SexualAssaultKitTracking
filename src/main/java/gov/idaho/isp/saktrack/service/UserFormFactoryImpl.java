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

package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.dto.AdminUserForm;
import gov.idaho.isp.saktrack.domain.user.dto.OrgUserForm;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.domain.user.password.dto.ChangePasswordRequest;
import gov.idaho.isp.saktrack.domain.user.password.dto.PasswordPair;
import gov.idaho.isp.saktrack.domain.user.password.dto.SimplePasswordPair;
import java.time.LocalDate;
import java.util.Optional;
import org.springframework.stereotype.Service;

@Service
public class UserFormFactoryImpl implements UserFormFactory {
  private final OrganizationUserRepository organizationUserRepository;
  private final AdminUserRepository adminUserRepository;
  private final OrganizationRepository organizationRepository;

  public UserFormFactoryImpl(OrganizationUserRepository organizationUserRepository, AdminUserRepository adminUserRepository, OrganizationRepository organizationRepository) {
    this.organizationUserRepository = organizationUserRepository;
    this.adminUserRepository = adminUserRepository;
    this.organizationRepository = organizationRepository;
  }

  @Override
  public OrgUserForm getOrgUserForm(Optional<Long> userId, Long organizationId, boolean hasCurrentPassword) {
    OrgUserForm orgUserForm = new OrgUserForm();
    orgUserForm.setOrgUser(loadOrCreateVerifiedUser(userId, organizationId));
    orgUserForm.setPasswordPair(preparePasswordPair(userId, hasCurrentPassword));
    return orgUserForm;
  }

  @Override
  public OrgUserForm getOrgUserForm(Optional<Long> organizationId) {
    OrgUserForm orgUserForm = new OrgUserForm();
    orgUserForm.setPasswordPair(new SimplePasswordPair());
    orgUserForm.setOrgUser(createUnverifiedUser(organizationId));
    return orgUserForm;
  }

  @Override
  public AdminUserForm getAdminUserForm(Optional<Long> userId, boolean hasCurrentPassword) {
    AdminUserForm adminUserForm = new AdminUserForm();
    adminUserForm.setPasswordPair(preparePasswordPair(userId, hasCurrentPassword));
    if (userId.isPresent()) {
      adminUserForm.setAdminUser(adminUserRepository.findById(userId.get()).orElse(null));
    }
    return adminUserForm;
  }

  private AbstractOrganizationUser loadOrCreateVerifiedUser(Optional<Long> userId, Long organizationId) {
    Organization organization = organizationRepository.findById(organizationId).orElse(null);
    AbstractOrganizationUser orgUser = userId.isPresent() ? loadOrganizationUser(userId.get()) : getNewUser(organization);
    orgUser.setOrganization(organization);
    orgUser.setPasskey(organization.getPasskey());
    if (orgUser.getVerifiedDate() == null) {
      orgUser.setVerifiedDate(LocalDate.now());
    }
    return orgUser;
  }

  private AbstractOrganizationUser createUnverifiedUser(Optional<Long> organizationId) {
    if (!organizationId.isPresent()) {
      return nullOrganizationUser();
    }
    Organization organization = organizationRepository.findById(organizationId.get()).orElse(null);
    AbstractOrganizationUser orgUser = getNewUser(organization);
    orgUser.setOrganization(organization);
    return orgUser;
  }

  private AbstractOrganizationUser nullOrganizationUser() {
    return new AbstractOrganizationUser() {
      @Override
      public User.Type getType() {
        return null;
      }
    };
  }

  private PasswordPair preparePasswordPair(Optional<Long> userId, boolean hasCurrentPassword) {
    if (hasCurrentPassword) {
      ChangePasswordRequest req = new ChangePasswordRequest();
      if (userId.isPresent()) {
        req.setUserId(userId.get());
      }
      return req;
    }

    return new SimplePasswordPair();
  }

  private AbstractOrganizationUser loadOrganizationUser(Long userId) {
    return organizationUserRepository.findById(userId).orElse(null);
  }

  private AbstractOrganizationUser getNewUser(Organization organization) {
    return organization.getType().getNewUser();
  }
}