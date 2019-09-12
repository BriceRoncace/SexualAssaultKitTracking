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

import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.domain.user.password.dto.ChangePasswordRequest;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;

public class OldPasswordPresentAndValidValidator implements ConstraintValidator<OldPasswordPresentAndValid, ChangePasswordRequest> {
  private OrganizationUserRepository organizationUserRepository;
  private PasswordEncoder passwordEncoder;

  @Override
  public void initialize(OldPasswordPresentAndValid constraintAnnotation) {
  }

  @Override
  public boolean isValid(ChangePasswordRequest req, ConstraintValidatorContext context) {
    if (req == null || passwordEncoder == null || organizationUserRepository == null) {
      return true;
    }

    if (!allBlankOrAllPresent(req.getOldPassword(), req.getPasswordOne(), req.getPasswordTwo())) {
      return false;
    }

    if (req.getOldPassword() != null && !passwordEncoder.matches(req.getOldPassword(), getCurrentPassword(req))) {
      return false;
    }

    return true;
  }

  private String getCurrentPassword(ChangePasswordRequest req) {
    OrganizationUser user = organizationUserRepository.findOneInNewTransaction(req.getUserId());
    return user.getPassword();
  }

  private boolean allBlankOrAllPresent(String... values) {
    return allBlank(values) || allPresent(values);
  }

  private boolean allBlank(String... values) {
    for (String val : values) {
      if (StringUtils.isNotBlank(val)) {
        return false;
      }
    }
    return true;
  }

  private boolean allPresent(String... values) {
    return StringUtils.isNoneBlank(values);
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setPasswordEncoder(PasswordEncoder passwordEncoder) {
    this.passwordEncoder = passwordEncoder;
  }
}