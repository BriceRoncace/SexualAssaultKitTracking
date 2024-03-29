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

import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.dto.UserForm;
import gov.idaho.isp.saktrack.domain.user.password.dto.PasswordPair;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

public class PasswordPresentIfRequiredValidator implements ConstraintValidator<PasswordPresentIfRequired, UserForm> {

  @Override
  public void initialize(PasswordPresentIfRequired a) {
  }

  @Override
  public boolean isValid(UserForm userForm, ConstraintValidatorContext cvc) {
    if (userForm == null || userForm.getAbstractUser() == null || userForm.getPasswordPair() == null) {
      return true;
    }

    AbstractUser user = userForm.getAbstractUser();
    PasswordPair passwordPair = userForm.getPasswordPair();
    return !(user.getId() == null && StringUtils.isBlank(passwordPair.getPasswordOne()));
  }
}
