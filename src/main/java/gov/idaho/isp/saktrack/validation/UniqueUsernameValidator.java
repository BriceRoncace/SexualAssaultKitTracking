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

package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.AdminUserRepository;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.util.reflection.PropertyUtils;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class UniqueUsernameValidator implements ConstraintValidator<UniqueUsername, Object> {
  private OrganizationUserRepository organizationUserRepository;
  private AdminUserRepository adminUserRepository;
  private String userProperty;

  @Override
  public void initialize(UniqueUsername a) {
    userProperty = a.userProperty();
  }

  @Override
  public boolean isValid(Object object, ConstraintValidatorContext cvc) {
    User user = getUser(object);
    if (organizationUserRepository != null && adminUserRepository != null) {
      if (isAlreadyAnOrgUser(user) || isAlreadyAnAdminUser(user)) {
        return false;
      }
    }
    return true;
  }

  private User getUser(Object object) {
    if (StringUtils.isNotBlank(userProperty)) {
      return PropertyUtils.getProperty(object, userProperty, User.class);
    }
    else if (object instanceof User) {
      return (User) object;
    }
    throw new IllegalStateException("UniqueUsername validation constraint misconfigured: Anotation must be applied to User type or specify user property as an annotation attribute.");
  }

  private boolean isAlreadyAnOrgUser(User user) {
    return differentEntities(organizationUserRepository.findByUsernameIgnoreCase(user.getUsername()), user);
  }

  private boolean isAlreadyAnAdminUser(User user) {
    return differentEntities(adminUserRepository.findByUsernameIgnoreCase(user.getUsername()), user);
  }

  private boolean differentEntities(User existingUser, User user) {
    return existingUser != null && !existingUser.getId().equals(user.getId());
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setAdminUserRepository(AdminUserRepository adminUserRepository) {
    this.adminUserRepository = adminUserRepository;
  }
}
